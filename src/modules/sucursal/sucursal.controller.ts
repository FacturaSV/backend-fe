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
import { SucursalService } from './sucursal.service';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { CreateSucursalDto as CreateDto } from './dto/create-sucursal.dto';
import { UpdateSucursalDto as UpdateDto } from './dto/update-sucursal.dto';

@Controller('sucursal')
export class SucursalController {
  private TABLA = 'sucursal';
  private readonly logger = new Logger(SucursalController.name);

  constructor(private readonly servicio: SucursalService) {}

  /**
   * Endpoint para obtener todas las sucursales con paginación.
   * Se espera que el front envíe los headers 'x-transaction-id' y 'x-tenant-schema'.
   */
  @Get()
  async findAll(
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
    @Query('page') page?: number,
    @Query('limit') limit?: number,
  ): Promise<ResponseDto<any>> {
    // Asigna valores predeterminados en caso de no recibirlos
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';
    const currentPage = page ? Number(page) : 1;
    const currentLimit = limit ? Number(limit) : 10;

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando todas las ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.servicio.findAll(currentPage, currentLimit);
      this.logger.log(
        `Transaction ID: ${currentTransactionId} - Consulta exitosa en el esquema ${currentTenantSchema}`,
      );
      return new ResponseDto(
        200,
        `Informacion de ${this.TABLA} procesada correctamente`,
        'success',
        result,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al procesar la información en el esquema ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo procesar la informacion de las ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para crear una nueva registro.
   * Se espera que el front envíe los headers 'x-transaction-id' y 'x-tenant-schema'.
   */
  @Post()
  @HttpCode(201)
  async create(
    @Body() createSucursalDto: CreateDto,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Creando ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.servicio.create(createSucursalDto);
      this.logger.log(
        `Transaction ID: ${currentTransactionId} - ${this.TABLA} creada correctamente en el esquema ${currentTenantSchema}`,
      );
      return new ResponseDto(
        201,
        `${this.TABLA} creada correctamente`,
        'success',
        result,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al crear la ${this.TABLA} en el esquema ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo crear la ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para eliminar una registro por su ID.
   */
  @Delete(':id')
  @HttpCode(200)
  async deleteSucursal(
    @Param('id') id: string,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Eliminando ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
    );

    try {
      await this.servicio.markAsInactive(+id);
      this.logger.log(
        `Transaction ID: ${currentTransactionId} - ${this.TABLA} con ID ${id} eliminada en el esquema ${currentTenantSchema}`,
      );
      return new ResponseDto(
        200,
        'Registro eliminado correctamente',
        'success',
        id,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al eliminar la ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo eliminar la ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para actualizar una registro por su ID.
   */
  @Put(':id')
  @HttpCode(200)
  async updateSucursal(
    @Param('id') id: string,
    @Body() updateSucursalDto: UpdateDto,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Actualizando ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
    );

    try {
      const updatedSucursal = await this.servicio.update(
        +id,
        updateSucursalDto,
      );

      this.logger.log(
        `Transaction ID: ${currentTransactionId} - ${this.TABLA} con ID ${id} actualizada en el esquema ${currentTenantSchema}`,
      );

      return new ResponseDto(
        200,
        `${this.TABLA} actualizada correctamente`,
        'success',
        updatedSucursal,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al actualizar la ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
        error,
      );

      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo actualizar la ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para obtener una registro por su ID.
   * Se espera que el front envíe los headers 'x-transaction-id' y 'x-tenant-schema'.
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
      `Transaction ID: ${currentTransactionId} - Consultando ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
    );

    let registro;
    try {
      registro = await this.servicio.findById(+id);
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error inesperado al consultar ${this.TABLA} con ID ${id}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo obtener la ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }

    // 🔹 Si Prisma devuelve `null`, lanzamos manualmente el error 404 **fuera del try-catch**
    if (!registro) {
      this.logger.warn(
        `Transaction ID: ${currentTransactionId} - ${this.TABLA} con ID ${id} no encontrada en el esquema ${currentTenantSchema}`,
      );

      throw new HttpException(
        new ResponseDto(404, `${this.TABLA} no encontrada`, 'error', null),
        HttpStatus.NOT_FOUND,
      );
    }

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consulta exitosa para ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
    );

    return new ResponseDto(
      200,
      `${this.TABLA} encontrada`,
      'success',
      registro,
    );
  }

  @Post('buscar')
  async buscarSucursales(
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
      `Transaction ID: ${currentTransactionId} - Consultando ${this.TABLA} con filtros en el esquema ${currentTenantSchema}`,
    );
    try {
      const result = await this.servicio.buscarDinamico(filtros);
      this.logger.log(
        `Transaction ID: ${currentTransactionId} - Búsqueda completada en el esquema ${currentTenantSchema} con ${result.length} resultados`,
      );
      return new ResponseDto(200, 'Búsqueda completada', 'success', result);
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al buscar ${this.TABLA} con filtros en el esquema ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo completar la búsqueda de ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }
}
