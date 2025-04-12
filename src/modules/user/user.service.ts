import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { CreateUserDto as CreateDto } from './dto/create-user.dto';
import { UpdateUserDto as UpdateDto } from './dto/update-user.dto';
import { EstadoRegistro, User } from '@prisma/client';
import { PaginatedResult } from 'src/common/model/dto/pagination.dto';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';

@Injectable()
export class UserService {
  isDebug = process.env.ESTADORT === 'DEBUG';
  constructor(private readonly prisma: PrismaService) {}
  /**
   * Crea una nueva registro en la base de datos.
   * @param CreateDto - Datos para el nuevo registro.
   * @returns EL registro creada.
   */
  async create(createDto: CreateDto): Promise<User> {
    return await this.prisma.user.create({
      data: { ...createDto },
    });
  }

  /**
   * Listar todas los registro con paginación
   */
  async findAll(page = 1, limit = 10): Promise<PaginatedResult<User>> {
    const skip = (page - 1) * limit;
    const total = await this.prisma.user.count();

    // Verificamos el entorno: si es DEBUG, incluimos todos; si no, filtramos
    const isDebug = process.env.ESTADORT === 'DEBUG';
    const listaRegistro = await this.prisma.user.findMany({
      where: {
        estadoRt: isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
      skip,
      take: limit,
      orderBy: { id: 'desc' },
    });

    return {
      data: listaRegistro,
      page,
      limit,
      total,
      totalPages: Math.ceil(total / limit),
    };
  }

  async update(id: number, updateDto: UpdateDto) {
    const registro = await this.prisma.user.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    if (!registro) {
      throw new HttpException('Registo no encontrado', HttpStatus.NOT_FOUND);
    }

    return await this.prisma.user.update({
      where: { id },
      data: {
        ...updateDto, // Solo actualiza los campos enviados en el body
        updatedAt: new Date(), // Opcional: registrar la última actualización
      },
    });
  }

  async markAsInactive(id: number): Promise<void> {
    const sucursal = await this.prisma.user.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    if (!sucursal) {
      throw new HttpException(
        new ResponseDto(404, 'Registo no encontrada', 'error', null),
        HttpStatus.NOT_FOUND,
      );
    }

    await this.prisma.user.delete({
      where: { id },
    });
  }

  async findById(id: number) {
    return await this.prisma.user.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });
  }

  async buscarDinamico(filtros: {
    AND?: { columna: string; operador: string; valor: any }[];
    OR?: { columna: string; operador: string; valor: any }[];
  }) {
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
      return await this.prisma.user.findMany();
    }

    const lastCondition = {
      ...condiciones,
      estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
    };

    return await this.prisma.user.findMany({
      where: lastCondition,
    });
  }
}
