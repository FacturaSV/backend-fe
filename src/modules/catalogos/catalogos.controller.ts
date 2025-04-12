import {
  Controller,
  Get,
  Logger,
  Headers,
  HttpException,
  HttpStatus,
} from '@nestjs/common';
import { CatalogosService } from './catalogos.service';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';

@Controller('catalogos')
export class CatalogosController {
  private TABLA = 'catalogos';
  private readonly logger = new Logger(CatalogosController.name);
  constructor(private readonly catalogosService: CatalogosService) {}
  // @Get()
  // findAll() {
  //   return this.catalogosService.findAll();
  // }

  @Get('/index')
  async findAllCatalogos(
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    // Asigna valores predeterminados en caso de no recibirlos
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando todas las ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.catalogosService.findAllCatalogos();
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

  @Get('/act-economica')
  async findAllActEconomica(
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    // Asigna valores predeterminados en caso de no recibirlos
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando todas las ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.catalogosService.findAllActEconomica();
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

  @Get('/dte')
  async findDte(
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    // Asigna valores predeterminados en caso de no recibirlos
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando todas las ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.catalogosService.findAllCatalogos();
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
}
