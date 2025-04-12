import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { CreateEmpresaDto as CreateDto } from './dto/create-empresa.dto';
import { UpdateEmpresaDto as UpdateDto } from './dto/update-empresa.dto';
import { Empresa, EstadoRegistro, Prisma } from '@prisma/client';
import { PaginatedResult } from 'src/common/model/dto/pagination.dto';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class EmpresaService {
  isDebug = process.env.ESTADORT === 'DEBUG';

  constructor(private readonly prisma: PrismaService) {}

  /**
   * Crea una nueva empresa en la base de datos.
   * @param createDto - Datos para la nueva empresa.
   * @returns La empresa creada.
   */
  async create(createDto: CreateDto): Promise<Empresa> {
    return await this.prisma.empresa.create({
      data: { ...createDto },
    });
  }

  /**
   * Lista todas las empresas con paginación
   */
  async findAll(
    page = 1,
    limit = 10,
    where?: Prisma.EmpresaWhereInput,
  ): Promise<PaginatedResult<Empresa>> {
    const skip = (page - 1) * limit;
    const total = await this.prisma.empresa.count({
      where: {
        ...where,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    const empresas = await this.prisma.empresa.findMany({
      skip,
      take: limit,
      orderBy: { id: 'desc' },
      where: {
        ...where,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    return {
      data: empresas,
      page,
      limit,
      total,
      totalPages: Math.ceil(total / limit),
    };
  }

  async findByGruop(keycloakGroupId: string): Promise<Empresa | null> {
    return await this.prisma.empresa.findFirst({
      where: {
        keycloakGroupId,
        ...(this.isDebug
          ? {}
          : { estadoRt: { not: EstadoRegistro.ELIMINADO } }),
      },
    });
  }

  /**
   * Busca una empresa por su ID
   */
  async findById(id: number): Promise<Empresa | null> {
    return await this.prisma.empresa.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });
  }

  /**
   * Actualiza una empresa por su ID.
   */
  async update(id: number, updateDto: UpdateDto): Promise<Empresa> {
    const empresa = await this.findById(id);

    if (!empresa) {
      throw new HttpException('Empresa no encontrada', HttpStatus.NOT_FOUND);
    }

    return await this.prisma.empresa.update({
      where: { id },
      data: {
        ...updateDto,
        updatedAt: new Date(), // Opcional: registrar la última actualización
      },
    });
  }

  /**
   * Marca una empresa como eliminada en lugar de borrarla completamente.
   */
  async markAsInactive(id: number): Promise<void> {
    const empresa = await this.findById(id);

    if (!empresa) {
      throw new HttpException(
        new ResponseDto(404, 'Empresa no encontrada', 'error', null),
        HttpStatus.NOT_FOUND,
      );
    }

    await this.prisma.empresa.update({
      where: { id },
      data: { estadoRt: EstadoRegistro.ELIMINADO },
    });
  }

  /**
   * Elimina completamente una empresa de la base de datos.
   */
  async delete(id: number): Promise<void> {
    const empresa = await this.findById(id);

    if (!empresa) {
      throw new HttpException('Empresa no encontrada', HttpStatus.NOT_FOUND);
    }

    await this.prisma.empresa.delete({ where: { id } });
  }

  /**
   * Búsqueda dinámica con operadores personalizados
   */
  async buscarDinamico(filtros: {
    AND?: { columna: string; operador: string; valor: any }[];
    OR?: { columna: string; operador: string; valor: any }[];
  }) {
    const condiciones: any = {};

    // Mapeo de operadores SQL a Prisma
    const operadorPrisma = {
      '=': 'equals',
      '!=': 'not',
      '>': 'gt',
      '>=': 'gte',
      '<': 'lt',
      '<=': 'lte',
      LIKE: 'contains',
    };

    // Procesar filtros AND
    if (filtros.AND && filtros.AND.length > 0) {
      condiciones.AND = filtros.AND.map((filtro) => {
        const operador = operadorPrisma[filtro.operador];

        const valorConvertido = isNaN(Number(filtro.valor))
          ? filtro.valor
          : Number(filtro.valor);

        if (operador === 'contains') {
          return {
            [filtro.columna]: {
              [operador]: valorConvertido,
              mode: 'insensitive',
            },
          };
        }
        return { [filtro.columna]: { [operador]: valorConvertido } };
      });
    }

    // Procesar filtros OR
    if (filtros.OR && filtros.OR.length > 0) {
      condiciones.OR = filtros.OR.map((filtro) => {
        const operador = operadorPrisma[filtro.operador];

        const valorConvertido = isNaN(Number(filtro.valor))
          ? filtro.valor
          : Number(filtro.valor);

        if (operador === 'contains') {
          return {
            [filtro.columna]: {
              [operador]: valorConvertido,
              mode: 'insensitive',
            },
          };
        }
        return { [filtro.columna]: { [operador]: valorConvertido } };
      });
    }

    // Si no hay filtros válidos, devolver todos los registros sin filtro
    if (!condiciones.AND && !condiciones.OR) {
      return await this.prisma.empresa.findMany();
    }

    const lastCondition = {
      ...condiciones,
      estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
    };

    return await this.prisma.empresa.findMany({
      where: lastCondition,
    });
  }

  async empresaInfoKeyCloak(domain: string): Promise<string> {
    try {
      const resultado = await this.prisma.$queryRaw<{ empresaInfo: string }[]>`
      SELECT facturalink.get_empresa_by_keycloak_group(${domain}::text) AS "empresaInfo";
    `;

      if (resultado.length > 0) {
        return resultado[0].empresaInfo;
      }
      return '';
    } catch (error) {
      console.error('Error al llamar al procedimiento almacenado:', error);
      return '';
    }
  }
}
