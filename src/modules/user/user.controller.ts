import {
  Controller,
  Get,
  Post,
  Body,
  Put,
  Delete,
  Param,
  HttpCode,
  Headers,
  HttpException,
  HttpStatus,
  Logger,
} from '@nestjs/common';
import { UserService } from './user.service';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';

@Controller('user')
export class UserController {
  private TABLA = 'usuario';
  private readonly logger = new Logger(UserController.name);

  constructor(private readonly userService: UserService) {}

  /**
   * ✅ Endpoint para obtener todos los usuarios con paginación.
   */
  @Get()
  async findAll(
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando todas las ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    try {
      const result = await this.userService.findAll();
      return new ResponseDto(
        200,
        `Información de ${this.TABLA} obtenida`,
        'success',
        result,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al obtener ${this.TABLA} en el esquema ${currentTenantSchema}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        new ResponseDto(500, `Error al obtener ${this.TABLA}`, 'error', null),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * ✅ Endpoint para obtener un usuario por su ID.
   */
  @Get(':id')
  async findOne(
    @Param('id') id: string,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Consultando ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
    );

    try {
      const user = await this.userService.findById(+id);
      if (!user) {
        this.logger.warn(
          `Transaction ID: ${currentTransactionId} - ${this.TABLA} con ID ${id} no encontrado en el esquema ${currentTenantSchema}`,
        );
        throw new HttpException(
          new ResponseDto(404, `${this.TABLA} no encontrado`, 'error', null),
          HttpStatus.NOT_FOUND,
        );
      }
      return new ResponseDto(200, `${this.TABLA} encontrado`, 'success', user);
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al obtener ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
        error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo obtener el ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  @Post()
  @HttpCode(201)
  async create(
    @Body() createUserDto: CreateUserDto,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Creando ${this.TABLA} en el esquema ${currentTenantSchema}`,
    );

    const result = await this.userService.create(createUserDto);

    return new ResponseDto(
      201,
      `${this.TABLA} creado correctamente`,
      'success',
      result,
    );
  }

  /**
   * ✅ Endpoint para actualizar un usuario por su ID.
   */
  @Put(':id')
  @HttpCode(200)
  async update(
    @Param('id') id: string,
    @Body() updateUserDto: UpdateUserDto,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    const currentTransactionId = transactionId || 'NO_TRANSACTION_ID';
    const currentTenantSchema = tenantSchema || 'default_schema';

    this.logger.log(
      `Transaction ID: ${currentTransactionId} - Actualizando ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
    );

    try {
      const updatedUser = await this.userService.update(+id, updateUserDto);
      return new ResponseDto(
        200,
        `${this.TABLA} actualizado correctamente`,
        'success',
        updatedUser,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al actualizar ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
        error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo actualizar el ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  /**
   * ✅ Endpoint para eliminar un usuario (marcar como eliminado).
   */
  @Delete(':id')
  @HttpCode(200)
  async remove(
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
      await this.userService.markAsInactive(+id);
      return new ResponseDto(
        200,
        `${this.TABLA} eliminado correctamente`,
        'success',
        id,
      );
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${currentTransactionId} - Error al eliminar ${this.TABLA} con ID ${id} en el esquema ${currentTenantSchema}`,
        error,
      );
      throw new HttpException(
        new ResponseDto(
          500,
          `No se pudo eliminar el ${this.TABLA}`,
          'error',
          null,
        ),
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }
}
