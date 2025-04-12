import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { CreateClientDto as CreateDto } from './dto/create-client.dto';
import { UpdateClientDto as UpdateDto } from './dto/update-client.dto';
import { Cliente, EstadoRegistro, Prisma } from '@prisma/client';
import { PaginatedResult } from 'src/common/model/dto/pagination.dto';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class ClientService {
  isDebug = process.env.ESTADORT === 'DEBUG';

  constructor(private readonly prisma: PrismaService) {}

  /**
   * Crea un nuevo cliente en la base de datos.
   * @param createDto - Datos para el nuevo cliente.
   * @returns Cliente creado.
   */
  async create(createDto: CreateDto): Promise<Cliente> {
    return await this.prisma.cliente.create({
      data: {
        ...createDto,
        estadoRt: createDto.estadoRt ?? EstadoRegistro.ACTIVO, // Estado por defecto
      },
    });
  }

  /**
   * Lista todos los clientes con paginación.
   */
  async findAll(
    page = 1,
    limit = 10,
    where?: Prisma.ClienteWhereInput,
  ): Promise<PaginatedResult<Cliente>> {
    const skip = (page - 1) * limit;
    const total = await this.prisma.cliente.count({
      where: {
        ...where,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    const listaClientes = await this.prisma.cliente.findMany({
      skip,
      take: limit,
      orderBy: { id: 'desc' },
      where: {
        ...where,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
    });

    return {
      data: listaClientes,
      page,
      limit,
      total,
      totalPages: Math.ceil(total / limit),
    };
  }

  /**
   * Obtiene un cliente por su ID.
   */
  async findById(id: number): Promise<Cliente | null> {
    return await this.prisma.cliente.findUnique({
      where: {
        id,
        estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
      },
      include: {
        municipio: true,
        departamento: true,
        ClienteActividadEconomica: {
          include: {
            actividadEconomica: true,
          },
        },
      },
    });
  }

  /**
   * Actualiza un cliente por su ID.
   */
  async update(id: number, updateDto: UpdateDto): Promise<Cliente> {
    const cliente = await this.findById(id);

    if (!cliente) {
      throw new HttpException('Cliente no encontrado', HttpStatus.NOT_FOUND);
    }

    return await this.prisma.cliente.update({
      where: { id },
      data: {
        ...updateDto,
        updatedAt: new Date(), // Registrar la última actualización
      },
    });
  }

  /**
   * Marca un cliente como inactivo en lugar de eliminarlo.
   */
  async markAsInactive(id: number): Promise<void> {
    const cliente = await this.findById(id);

    if (!cliente) {
      throw new HttpException(
        new ResponseDto(404, 'Cliente no encontrado', 'error', null),
        HttpStatus.NOT_FOUND,
      );
    }

    await this.prisma.cliente.update({
      where: { id },
      data: { estadoRt: EstadoRegistro.INACTIVO },
    });
  }

  /**
   * Elimina un cliente permanentemente de la base de datos.
   */
  async delete(id: number): Promise<void> {
    const cliente = await this.findById(id);

    if (!cliente) {
      throw new HttpException('Cliente no encontrado', HttpStatus.NOT_FOUND);
    }

    await this.prisma.cliente.delete({ where: { id } });
  }

  /**
   * Búsqueda dinámica con operadores personalizados.
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
      return await this.prisma.cliente.findMany();
    }

    const lastCondition = {
      ...condiciones,
      estadoRt: this.isDebug ? undefined : { not: EstadoRegistro.ELIMINADO },
    };

    return await this.prisma.cliente.findMany({
      where: lastCondition,
    });
  }

  async buscarClientesPorEmpresa(
    empresaId: number,
    page = 1,
    limit = 10,
    filtros: {
      AND?: { columna: string; operador: string; valor: any }[];
      OR?: { columna: string; operador: string; valor: any }[];
    },
  ): Promise<PaginatedResult<Cliente>> {
    const skip = (page - 1) * limit;

    const operadorPrisma: Record<string, string> = {
      '=': 'equals',
      '!=': 'not',
      '>': 'gt',
      '>=': 'gte',
      '<': 'lt',
      '<=': 'lte',
      LIKE: 'contains',
    };

    const condicionesCliente: any = {};

    const filtrosValidos = (
      filtros?: { columna: string; operador: string; valor: any }[],
    ) => filtros?.filter((f) => operadorPrisma[f.operador]) || [];

    const relacionesUnoAUno = new Set([
      'municipio',
      'departamento',
      'actividadEconomica',
    ]);

    const mapFiltro = (f: {
      columna: string;
      operador: string;
      valor: any;
    }) => {
      const operador = operadorPrisma[f.operador];
      const valor =
        typeof f.valor === 'string'
          ? f.valor
          : isNaN(Number(f.valor))
            ? f.valor
            : Number(f.valor);

      const partes = f.columna.split('.');

      if (partes.length > 1) {
        const campoFinal = partes.pop()!;
        const relaciones = partes; // no uses reverse()

        let filtro: any = {
          [campoFinal]: {
            [operador]: valor,
            ...(f.operador === 'LIKE' ? { mode: 'insensitive' } : {}),
          },
        };

        // construimos de adentro hacia afuera
        for (let i = relaciones.length - 1; i >= 0; i--) {
          const relacion = relaciones[i];
          filtro = {
            [relacion]: relacionesUnoAUno.has(relacion)
              ? { is: filtro }
              : { some: filtro },
          };
        }

        return filtro;
      }

      return {
        [f.columna]: {
          [operador]: valor,
          ...(f.operador === 'LIKE' ? { mode: 'insensitive' } : {}),
        },
      };
    };

    const filtrosAND = filtrosValidos(filtros.AND);
    const filtrosOR = filtrosValidos(filtros.OR);

    if (filtrosAND.length) {
      condicionesCliente.AND = filtrosAND.map(mapFiltro);
    }

    if (filtrosOR.length) {
      condicionesCliente.OR = filtrosOR.map(mapFiltro);
    }

    condicionesCliente.estadoRt = this.isDebug
      ? undefined
      : { not: EstadoRegistro.ELIMINADO };

    const total = await this.prisma.empresaClientes.count({
      where: {
        empresaId,
        cliente: condicionesCliente,
      },
    });

    const registros = await this.prisma.empresaClientes.findMany({
      where: {
        empresaId,
        cliente: condicionesCliente,
      },
      include: {
        cliente: {
          include: {
            municipio: true,
            departamento: true,
            ClienteActividadEconomica: {
              include: {
                actividadEconomica: true,
              },
            },
          },
        },
      },
      skip,
      take: limit,
      orderBy: {
        clienteId: 'desc',
      },
    });

    const clientes = registros.map((r) => r.cliente);

    return {
      data: clientes,
      page,
      limit,
      total,
      totalPages: Math.ceil(total / limit),
    };
  }
}
