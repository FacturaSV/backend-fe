import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { CreateProductoCategoriaDto as CreateDto } from './dto/create-producto-categoria.dto';
import { UpdateProductoCategoriaDto as UpdateDto } from './dto/update-producto-categoria.dto';
import { EstadoRegistro, Prisma, ProductoCategoria } from '@prisma/client';
import { PaginatedResult } from 'src/common/model/dto/pagination.dto';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class ProductoCategoriaService {
  isDebug = process.env.ESTADORT === 'DEBUG';
  includeConfig = { producto: true, categoria: false };

  constructor(private readonly prisma: PrismaService) {}
  /**
   * Crea una nueva registro en la base de datos.
   * @param CreateDto - Datos para el nuevo registro.
   * @returns EL registro creada.
   */
  async create(createDto: CreateDto): Promise<ProductoCategoria> {
    return await this.prisma.productoCategoria.create({
      data: { ...createDto },
    });
  }

  /**
   * Listar todas los registro con paginación
   */
  async findAll(
    page = 1,
    limit = 10,
    where?: Prisma.ProductoCategoriaWhereInput,
  ): Promise<PaginatedResult<ProductoCategoria>> {
    const skip = (page - 1) * limit;
    const total = await this.prisma.productoCategoria.count({
      where: {
        // Aplica el mismo where para contar
        ...where,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    const listaRegistro = await this.prisma.productoCategoria.findMany({
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
    const registro = await this.prisma.productoCategoria.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    if (!registro) {
      throw new HttpException('Registo no encontrado', HttpStatus.NOT_FOUND);
    }

    return await this.prisma.productoCategoria.update({
      where: { id },
      data: {
        ...updateDto, // Solo actualiza los campos enviados en el body
        updatedAt: new Date(), // Opcional: registrar la última actualización
      },
    });
  }

  async markAsInactive(id: number): Promise<void> {
    const sucursal = await this.prisma.productoCategoria.findUnique({
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

    await this.prisma.productoCategoria.delete({
      where: { id },
    });
  }

  async findById(id: number) {
    return await this.prisma.productoCategoria.findUnique({
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
      return await this.prisma.productoCategoria.findMany({
        include: this.includeConfig,
      });
    }

    const lastCondition = {
      ...condiciones,
      estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
    };

    return await this.prisma.productoCategoria.findMany({
      include: this.includeConfig,
      where: lastCondition,
    });
  }
}
