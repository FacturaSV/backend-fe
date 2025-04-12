import {
  Controller,
  Get,
  Logger,
  Query,
  Headers,
  HttpException,
  HttpStatus,
  Body,
  HttpCode,
  Post,
  Delete,
  Param,
  Put,
} from '@nestjs/common';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { ClientService } from './client.service';
import { CreateClientDto as CreateDto } from './dto/create-client.dto';
import { UpdateClientDto as UpdateDto } from './dto/update-client.dto';

@Controller('cliente')
export class ClientController {
  private TABLA = 'cliente';
  private readonly logger = new Logger(ClientController.name);

  constructor(private readonly servicio: ClientService) {}

  /**
   * Endpoint para obtener todos los clientes con paginación.
   */
  @Get()
  async findAll(
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
    @Query('page') page?: number,
    @Query('limit') limit?: number,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';
    const currentPage = page ? Number(page) : 1;
    const currentLimit = limit ? Number(limit) : 10;

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando todos los clientes en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.servicio.findAll(currentPage, currentLimit);
      return new ResponseDto(
        200,
        `Información de clientes procesada correctamente`,
        'success',
        result,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al consultar clientes en ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo obtener la información de los clientes`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para crear un nuevo cliente.
   */
  @Post()
  @HttpCode(201)
  async create(
    @Body() createClienteDto: CreateDto,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Creando cliente en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.servicio.create(createClienteDto);
      return new ResponseDto(
        201,
        `Cliente creado correctamente`,
        'success',
        result,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al crear cliente en ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(500, `No se pudo crear el cliente`, 'error', null),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para obtener un cliente por su ID.
   */
  @Get(':id')
  async findById(
    @Param('id') id: string,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando cliente con ID ${id} en ${currentTenantSchema}`,
    );

    try {
      const cliente = await this.servicio.findById(+id);
      if (!cliente) {
        throw new HttpException(
          new ResponseDto(404, `Cliente no encontrado`, 'error', null),
          HttpStatus.NOT_FOUND,
        );
      }

      return new ResponseDto(200, `Cliente encontrado`, 'success', cliente);
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al consultar cliente con ID ${id} en ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(500, `No se pudo obtener el cliente`, 'error', null),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para actualizar un cliente por su ID.
   */
  @Put(':id')
  @HttpCode(200)
  async update(
    @Param('id') id: string,
    @Body() updateClienteDto: UpdateDto,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Actualizando cliente con ID ${id} en ${currentTenantSchema}`,
    );

    try {
      const updatedCliente = await this.servicio.update(+id, updateClienteDto);
      return new ResponseDto(
        200,
        `Cliente actualizado correctamente`,
        'success',
        updatedCliente,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al actualizar cliente con ID ${id} en ${currentTenantSchema}`,
        error,
      );

      throw new HttpException(
        new ResponseDto(500, `No se pudo actualizar el cliente`, 'error', null),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para eliminar un cliente por su ID.
   */
  @Delete(':id')
  @HttpCode(200)
  async delete(
    @Param('id') id: string,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Eliminando cliente con ID ${id} en ${currentTenantSchema}`,
    );

    try {
      await this.servicio.markAsInactive(+id);
      return new ResponseDto(
        200,
        'Cliente eliminado correctamente',
        'success',
        id,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al eliminar cliente con ID ${id} en ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(500, `No se pudo eliminar el cliente`, 'error', null),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para realizar búsquedas dinámicas de clientes.
   */
  @Post('buscar')
  async buscarClientes(
    @Body()
    filtros: {
      AND?: { columna: string; operador: string; valor: string }[];
      OR?: { columna: string; operador: string; valor: string }[];
    },
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ) {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Buscando clientes con filtros en ${currentTenantSchema}`,
    );

    try {
      const result = await this.servicio.buscarDinamico(filtros);
      return new ResponseDto(200, 'Búsqueda completada', 'success', result);
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error en la búsqueda de clientes en ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo completar la búsqueda de clientes`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  @Post('/empresa')
  async findClientFacturados(
    @Body()
    filtros: {
      AND?: { columna: string; operador: string; valor: string }[];
      OR?: { columna: string; operador: string; valor: string }[];
    },
    @Query('empresa') empresaId: number,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
    @Query('page') page?: number,
    @Query('limit') limit?: number,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';
    const currentPage = page ? Number(page) : 1;
    const currentLimit = limit ? Number(limit) : 10;

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando todos los clientes en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.servicio.buscarClientesPorEmpresa(
        empresaId,
        currentPage,
        currentLimit,
        filtros,
      );
      return new ResponseDto(
        200,
        `Información de clientes procesada correctamente`,
        'success',
        result,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al consultar clientes en ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo obtener la información de los clientes`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }
}
