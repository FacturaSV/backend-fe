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
  Patch,
} from "@nestjs/common";
import { ResponseDto } from "src/common/model/dto/response.body.dto";
import { EmpresaService } from "./empresa.service";
import { CreateEmpresaDto as CreateDto } from "./dto/create-empresa.dto";
import { UpdateEmpresaDto as UpdateDto } from "./dto/update-empresa.dto";

@Controller("empresa")
export class EmpresaController {
  private TABLA = "empresa";
  private readonly logger = new Logger(EmpresaController.name);

  constructor(private readonly servicio: EmpresaService) {}

  @Get()
  async findAll(
    @Headers("x-transaction-id") transactionId?: string,
    @Headers("x-tenant-schema") tenantSchema?: string,
    @Query("page") page?: number,
    @Query("limit") limit?: number,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || "NO_TRANSACTION_ID";
    const currentTenantSchema = tenantSchema || "default_schema";
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
        `Información de ${this.TABLA} procesada correctamente`,
        "success",
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
          `No se pudo procesar la información de las ${this.TABLA}`,
          "error",
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  @Get("/token")
  async empresaToken(
    @Headers("x-transaction-id") transactionId?: string,
    @Headers("x-tenant-schema") tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || "NO_TRANSACTION_ID";
    const currentTenantSchema = tenantSchema || "default_schema";

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando todas las ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    try {
      const result =
        await this.servicio.empresaInfoKeyCloak(currentTenantSchema);
      this.logger.log(
        `Transaction ID: ${currentTransactionId} - Consulta exitosa en el esquema ${currentTenantSchema}`,
      );
      return new ResponseDto(
        200,
        `Información de ${this.TABLA} procesada correctamente`,
        "success",
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
          `No se pudo procesar la información de las ${this.TABLA}`,
          "error",
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para crear una nueva empresa.
   */
  @Post()
  @HttpCode(201)
  async create(
    @Body() createEmpresaDto: CreateDto,
    @Headers("x-transaction-id") transactionId?: string,
    @Headers("x-tenant-schema") tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || "NO_TRANSACTION_ID";
    const currentTenantSchema = tenantSchema || "default_schema";

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Creando ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.servicio.create(createEmpresaDto);
      this.logger.log(
        `Transaction ID: ${currentTransactionId} - ${this.TABLA} creada correctamente en el esquema ${currentTenantSchema}`,
      );
      return new ResponseDto(
        201,
        `${this.TABLA} creada correctamente`,
        "success",
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
          "error",
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para eliminar una empresa por su ID.
   */
  @Delete(":id")
  @HttpCode(200)
  async deleteEmpresa(
    @Param("id") id: string,
    @Headers("x-transaction-id") transactionId?: string,
    @Headers("x-tenant-schema") tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || "NO_TRANSACTION_ID";
    const currentTenantSchema = tenantSchema || "default_schema";

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Eliminando ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
    );

    try {
      await this.servicio.delete(+id);
      this.logger.log(
        `Transaction ID: ${currentTransactionId} - ${this.TABLA} con ID ${id} eliminada en el esquema ${currentTenantSchema}`,
      );
      return new ResponseDto(
        200,
        "Registro eliminado correctamente",
        "success",
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
          "error",
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para actualizar una empresa por su ID.
   */
  @Put(":id")
  @HttpCode(200)
  async updateEmpresa(
    @Param("id") id: string,
    @Body() updateEmpresaDto: UpdateDto,
    @Headers("x-transaction-id") transactionId?: string,
    @Headers("x-tenant-schema") tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || "NO_TRANSACTION_ID";
    const currentTenantSchema = tenantSchema || "default_schema";

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Actualizando ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
    );

    try {
      const updatedEmpresa = await this.servicio.update(+id, updateEmpresaDto);

      this.logger.log(
        `Transaction ID: ${currentTransactionId} - ${this.TABLA} con ID ${id} actualizada en el esquema ${currentTenantSchema}`,
      );

      return new ResponseDto(
        200,
        `${this.TABLA} actualizada correctamente`,
        "success",
        updatedEmpresa,
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
          "error",
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * Endpoint para obtener una empresa por su ID.
   */
  @Get(":id")
  async findById(
    @Param("id") id: string,
    @Headers("x-transaction-id") transactionId?: string,
    @Headers("x-tenant-schema") tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || "NO_TRANSACTION_ID";
    const currentTenantSchema = tenantSchema || "default_schema";

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
          "error",
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }

    if (!registro) {
      throw new HttpException(
        new ResponseDto(404, `${this.TABLA} no encontrada`, "error", null),
        HttpStatus.NOT_FOUND,
      );
    }

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consulta exitosa para ${this.TABLA} con ID ${id}`,
    );
    return new ResponseDto(
      200,
      `${this.TABLA} encontrada`,
      "success",
      registro,
    );
  }

  /**
   * Endpoint para obtener una empresa por su ID.
   */
  @Patch("")
  async findByGroup(
    @Headers("x-transaction-id") transactionId?: string,
    @Headers("x-tenant-schema") tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || "NO_TRANSACTION_ID";
    const currentTenantSchema = tenantSchema || "default_schema";
    const group = tenantSchema || "default_schema";
    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando group ${this.TABLA} con group ${group} en el esquema ${currentTenantSchema}`,
    );

    let registro;
    try {
      registro = await this.servicio.findByGruop(group);
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error inesperado al consultar ${this.TABLA} con ID ${group}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo obtener la ${this.TABLA}`,
          "error",
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }

    if (!registro) {
      throw new HttpException(
        new ResponseDto(404, `${this.TABLA} no encontrada`, "error", null),
        HttpStatus.NOT_FOUND,
      );
    }

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consulta exitosa para ${this.TABLA} con ID ${group}`,
    );
    return new ResponseDto(
      200,
      `${this.TABLA} encontrada`,
      "success",
      registro,
    );
  }
}
