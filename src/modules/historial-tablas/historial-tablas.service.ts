import { Injectable } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
export enum AccionHistorial {
  CREATE = 'CREATE',
  UPDATE = 'UPDATE',
  DELETE = 'DELETE',
}

@Injectable()
export class HistorialTablasService {
  constructor(private readonly prisma: PrismaService) {}

  async registrarCambio(
    tabla: string,
    registroId: number,
    accion: AccionHistorial,
    usuario: number,
    datos: any,
  ) {
    await this.prisma.historialCambios.create({
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
