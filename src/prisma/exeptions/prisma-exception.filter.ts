import {
  ExceptionFilter,
  Catch,
  ArgumentsHost,
  HttpStatus,
  Logger,
} from '@nestjs/common';
import { Response } from 'express';
import { Prisma } from '@prisma/client';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';

@Catch(Prisma.PrismaClientKnownRequestError)
export class PrismaExceptionFilter implements ExceptionFilter {
  private readonly logger = new Logger(PrismaExceptionFilter.name);

  catch(exception: Prisma.PrismaClientKnownRequestError, host: ArgumentsHost) {
    const ctx = host.switchToHttp();
    const response = ctx.getResponse<Response>();

    let status = HttpStatus.INTERNAL_SERVER_ERROR;
    let message = 'Error desconocido en la base de datos';

    switch (exception.code) {
      case 'P2002': {
        // Clave única duplicada
        status = HttpStatus.BAD_REQUEST;
        const campoDuplicado =
          exception.meta?.target?.[0] || 'Campo desconocido';
        message = `El valor para <${campoDuplicado}> ya está en uso.`;
        break;
      }

      case 'P2025': {
        // Registro no encontrado
        status = HttpStatus.NOT_FOUND;
        message = 'El recurso solicitado no fue encontrado.';
        break;
      }

      case 'P2021': {
        // Tabla no existe
        status = HttpStatus.BAD_REQUEST;
        message = 'La tabla solicitada no existe en la base de datos.';
        break;
      }

      default:
        status = HttpStatus.INTERNAL_SERVER_ERROR;
        this.logger.error(
          `Error desconocido de Prisma: ${exception.message}`,
          exception.stack,
        );
        message = `Error inesperado en la base de datos: ${exception.message}`;
    }

    this.logger.warn(`Error de BD: ${message}`);

    response
      .status(status)
      .json(new ResponseDto(status, message, 'error', null));
  }
}
