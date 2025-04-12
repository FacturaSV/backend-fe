import { Injectable, Logger } from '@nestjs/common';
import { TransaccionesDTE } from '@prisma/client';
import { TransaccionesDteDto } from '../cron-job/cron-job-dte.dto';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class TrxDteService {
  private readonly logger = new Logger(TrxDteService.name);

  constructor(private readonly prisma: PrismaService) {}

  async buscarDinamicoTrx(filtros: {
    AND?: { columna: string; operador: string; valor: any }[];
    OR?: { columna: string; operador: string; valor: any }[];
  }): Promise<TransaccionesDTE[]> {
    const condiciones: any = {};

    // Función para mapear operadores SQL a Prisma
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

        // Convertimos los valores a números si es posible
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

        // Convertimos los valores a números si es posible
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
      return await this.prisma.transaccionesDTE.findMany();
    }

    return await this.prisma.transaccionesDTE.findMany({
      where: condiciones,
    });
  }

  async createTrxDte(
    createDto: TransaccionesDteDto,
  ): Promise<TransaccionesDTE> {
    return await this.prisma.transaccionesDTE.create({
      data: { ...createDto },
    });
  }
}
