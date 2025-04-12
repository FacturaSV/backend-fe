/* eslint-disable @typescript-eslint/no-unused-vars */
import { REQUEST } from '@nestjs/core';
import { Request } from 'express';
import {
  Injectable,
  OnModuleInit,
  OnModuleDestroy,
  Inject,
  Logger,
} from '@nestjs/common';
import {
  AccionHistorial,
  EstadoRegistro,
  Prisma,
  PrismaClient,
} from '@prisma/client';

@Injectable()
export class PrismaService
  extends PrismaClient
  implements OnModuleInit, OnModuleDestroy
{
  private readonly logger = new Logger(PrismaService.name);
  private usuarioId: number | null;

  constructor(@Inject(REQUEST) request: Request) {
    // Determinamos el esquema según la petición (multi-tenant).
    // Si no existe, usamos 'backend_schema' por defecto.
    let schema = 'el_nazareno';
    // if (!['/empresa', '/cliente', '/catalogos'].includes(request.path)) {
    //   schema = (request['tenant-schema'] as string) ?? 'backend_schema';
    //   console.log('schema includes', schema);
    // }
    console.log('schema PrismaService', schema);
    const historial = (request['historial-active'] as string) ?? false;
    const url = process.env.DATABASE_URL?.replace('public', schema) ?? '';

    super({
      // datasources: {
      //   db: {
      //     url,
      //   },
      // },
    });
    this.usuarioId = +request['x-user'] || null;
    // this.setupMiddlewareAtUser();
    // if (historial) {
    //   this.setupMiddlewareHistorial();
    // }
  }

  async onModuleInit() {
    await this.$connect();
  }

  async onModuleDestroy() {
    await this.$disconnect();
  }

  private setupMiddlewareAtUser() {
    this.$use(async (params, next) => {
      const tablasConUsuariosAudit = ['HistorialCambios', 'User'];
      if (
        typeof params.model === 'string' &&
        tablasConUsuariosAudit.includes(params.model)
      ) {
        return next(params); // Evitar la recursión infinita
      }

      // **Manejar `FIND`**
      // if (['findMany', 'findUnique'].includes(params.action)) {
      //   const resultado = await next(params);

      //   const ocultarCampos = (obj: any) => {
      //     if (Array.isArray(obj)) {
      //       return obj.map((item) => ocultarCampos(item)); // Iterar sobre arrays
      //     } else if (typeof obj === 'object' && obj !== null) {
      //       const { createdBy, updatedBy, deletedBy, ...rest } = obj;
      //       return rest;
      //     }
      //     return obj;
      //   };
      //   return ocultarCampos(resultado);
      // }

      // **Manejar `UPDATE`**
      if (params.action === 'update') {
        params.args.data.updatedBy = this.usuarioId;
      }

      // **Manejar `CREATE`**
      if (params.action === 'create') {
        params.args.data.createdBy = this.usuarioId;
      }

      // **Manejar `DELETE`**
      if (params.action === 'delete') {
        // Si la tabla tiene `deleteUser`, lo llenamos automáticamente
        params.action = 'update'; // Cambia `DELETE` por `UPDATE` para actualizar el campo
        params.args.data = {
          deletedBy: this.usuarioId,
          estadoRt: EstadoRegistro.ELIMINADO,
        };
      }
      return next(params);
    });
  }

  private setupMiddlewareHistorial() {
    this.$use(async (params, next) => {
      if (params.model === 'HistorialCambios') {
        return next(params); // Evitar la recursión infinita
      }

      let antes = null;

      // Obtener el estado antes del cambio solo en `UPDATE`
      if (params.action === 'update') {
        antes = await this.getBeforeState(params);

        //  Si no hay cambios en `UPDATE`, no registrar en `HistorialCambios`
        if (JSON.stringify(antes) === JSON.stringify(params.args.data)) {
          this.logger.log(
            'No hay cambios en el estado, no se registrará en HistorialCambios',
          );
          return next(params);
        }
      }

      const resultado = await next(params);

      if (['create', 'update'].includes(params.action)) {
        const model = params.model ? params.model.toLowerCase() : 'desconocido';
        const registroId =
          params.action === 'create' ? resultado.id : params.args.where?.id;

        await this.registrarCambio(
          model,
          +registroId,
          params.action.toUpperCase() as AccionHistorial,
          0, // Puedes reemplazar esto con el usuario real
          { antes }, // **Solo guarda el estado antes del cambio**
        );
      }

      return resultado;
    });
  }

  private async getBeforeState(params: Prisma.MiddlewareParams) {
    const model = params.model === undefined ? '' : params.model.toLowerCase();
    return await this[model].findUnique({
      where: { id: params.args.where.id },
    });
  }

  private async registrarCambio(
    tabla: string,
    registroId: number,
    accion: AccionHistorial,
    usuario: number,
    datos: any,
  ) {
    await this.historialCambios.create({
      data: {
        tabla,
        registroId,
        accion,
        usuario,
        datos,
      },
    });
  }
}
