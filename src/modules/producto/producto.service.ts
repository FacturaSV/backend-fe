import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { CreateProductoDto as CreateDto } from './dto/create-producto.dto';
import { UpdateProductoDto as UpdateDto } from './dto/update-producto.dto';
import { Producto, EstadoRegistro, Prisma } from '@prisma/client';
import { PaginatedResult } from 'src/common/model/dto/pagination.dto';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class ProductoService {
  isDebug = process.env.ESTADORT === 'DEBUG';
  includeConfig = {};
  constructor(private readonly prisma: PrismaService) {}
  /**
   * Crea una nueva registro en la base de datos.
   * @param CreateDto - Datos para el nuevo registro.
   * @returns EL registro creada.
   */
  async create(createDto: CreateDto): Promise<Producto> {
    return await this.prisma.producto.create({
      data: { ...createDto },
    });
  }

  /**
   * Listar todas los registro con paginación
   */
  async findAll(
    page = 1,
    limit = 10,
    where?: Prisma.ProductoWhereInput,
  ): Promise<PaginatedResult<Producto>> {
    const skip = (page - 1) * limit;
    const total = await this.prisma.producto.count({
      where: {
        // Aplica el mismo where para contar
        ...where,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    const listaRegistro = await this.prisma.producto.findMany({
      skip,
      take: limit,
      orderBy: { id: 'desc' },
      include: this.includeConfig,
      where: {
        ...where,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
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
    const registro = await this.prisma.producto.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    if (!registro) {
      throw new HttpException('Registo no encontrado', HttpStatus.NOT_FOUND);
    }

    return await this.prisma.producto.update({
      where: { id },
      data: {
        ...updateDto, // Solo actualiza los campos enviados en el body
        updatedAt: new Date(), // Opcional: registrar la última actualización
      },
    });
  }

  async markAsInactive(id: number): Promise<void> {
    const sucursal = await this.prisma.producto.findUnique({
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

    await this.prisma.producto.delete({
      where: { id },
    });
  }

  async findById(id: number) {
    return await this.prisma.producto.findUnique({
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
      return await this.prisma.producto.findMany();
    }

    const lastCondition = {
      ...condiciones,
      estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
    };

    return await this.prisma.producto.findMany({
      where: lastCondition,
    });
  }
}
