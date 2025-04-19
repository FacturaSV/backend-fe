import { HttpException, HttpStatus, Injectable } from "@nestjs/common";
import { CreateSucursalDto as CreateDto } from "./dto/create-sucursal.dto";
import { UpdateSucursalDto as UpdateDto } from "./dto/update-sucursal.dto";
import { PrismaService } from "src/prisma/prisma.service";
import { EstadoRegistro, Prisma, Sucursal } from "@prisma/client";
import { PaginatedResult } from "src/common/model/dto/pagination.dto";
import { ResponseDto } from "src/common/model/dto/response.body.dto";

@Injectable()
export class SucursalService {
  isDebug = process.env.ESTADORT === "DEBUG";
  constructor(private readonly prisma: PrismaService) {}
  /**
   * Crea una nueva registro en la base de datos.
   * @param CreateDto - Datos para el nuevo regisztro.
   * @returns EL registro creada.
   */
  async create(createDto: CreateDto): Promise<Sucursal> {
    return await this.prisma.sucursal.create({
      data: { ...createDto },
    });
  }

  /**
   * Listar todas los registro con paginación
   */
  async findAll(
    page = 1,
    limit = 10,
    where?: Prisma.SucursalWhereInput,
  ): Promise<PaginatedResult<Sucursal>> {
    const skip = (page - 1) * limit;
    const total = await this.prisma.sucursal.count({
      where: {
        ...where,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    const isDebug = process.env.ESTADORT === "DEBUG";
    const listaRegistro = await this.prisma.sucursal.findMany({
      where: {
        ...where,
        estadoRt: isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
      skip,
      take: limit,
      orderBy: { id: "desc" },
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
    const registro = await this.prisma.sucursal.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    if (!registro) {
      throw new HttpException("Registo no encontrado", HttpStatus.NOT_FOUND);
    }

    return await this.prisma.sucursal.update({
      where: { id },
      data: {
        ...updateDto, // Solo actualiza los campos enviados en el body
        updatedAt: new Date(), // Opcional: registrar la última actualización
      },
    });
  }

  async markAsInactive(id: number): Promise<void> {
    const sucursal = await this.prisma.sucursal.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    if (!sucursal) {
      throw new HttpException(
        new ResponseDto(404, "Registo no encontrada", "error", null),
        HttpStatus.NOT_FOUND,
      );
    }

    await this.prisma.sucursal.delete({
      where: { id },
    });
  }

  async findById(id: number) {
    return await this.prisma.sucursal.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });
  }

  async buscarDinamico(
    filtros: {
      AND?: { columna: string; operador: string; valor: any }[];
      OR?: { columna: string; operador: string; valor: any }[];
    },
    page = 1,
    limit = 10,
    where?: Prisma.SucursalWhereInput,
  ) {
    const condiciones: any = {};
    const operadorPrisma = {
      "=": "equals",
      "!=": "not",
      ">": "gt",
      ">=": "gte",
      "<": "lt",
      "<=": "lte",
      LIKE: "contains",
    };

    // Procesar filtros AND
    if (filtros.AND?.length) {
      condiciones.AND = filtros.AND.map((filtro) => {
        const operador = operadorPrisma[filtro.operador];
        const valor = isNaN(Number(filtro.valor))
          ? filtro.valor
          : Number(filtro.valor);

        if (operador === "contains") {
          return {
            [filtro.columna]: {
              [operador]: valor,
              mode: "insensitive",
            },
          };
        }

        return { [filtro.columna]: { [operador]: valor } };
      });
    }

    // Procesar filtros OR
    if (filtros.OR?.length) {
      condiciones.OR = filtros.OR.map((filtro) => {
        const operador = operadorPrisma[filtro.operador];
        const valor = isNaN(Number(filtro.valor))
          ? filtro.valor
          : Number(filtro.valor);

        if (operador === "contains") {
          return {
            [filtro.columna]: {
              [operador]: valor,
              mode: "insensitive",
            },
          };
        }

        return { [filtro.columna]: { [operador]: valor } };
      });
    }

    const finalWhere: Prisma.SucursalWhereInput = {
      ...where,
      ...condiciones,
      estadoRt: this.isDebug ? undefined : { not: "ELIMINADO" },
    };

    const total = await this.prisma.sucursal.count({ where: finalWhere });

    const data = await this.prisma.sucursal.findMany({
      where: finalWhere,
      skip: (page - 1) * limit,
      take: limit,
      orderBy: { createdAt: "desc" }, // Opcional
    });

    return {
      data,
      total,
      page,
      limit,
      totalPages: Math.ceil(total / limit),
    };
  }
}
