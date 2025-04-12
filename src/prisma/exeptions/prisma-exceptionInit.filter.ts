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

@Catch(Prisma.PrismaClientInitializationError)
export class PrismaExceptionFilterInit implements ExceptionFilter {
  private readonly logger = new Logger(PrismaExceptionFilterInit.name);

  catch(
    exception: Prisma.PrismaClientInitializationError,
    host: ArgumentsHost,
  ) {
    const ctx = host.switchToHttp();
    const response = ctx.getResponse<Response>();

    this.logger.error(
      `Error de conexión a la base de datos: ${exception.message}`,
    );

    response
      .status(HttpStatus.SERVICE_UNAVAILABLE)
      .json(
        new ResponseDto(
          503,
          'No se pudo conectar db contacte al administrador',
          'error',
          null,
        ),
      );
  }
}
