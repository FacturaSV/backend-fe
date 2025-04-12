import {
  Body,
  Controller,
  HttpException,
  HttpStatus,
  Headers,
  Logger,
  Post,
} from '@nestjs/common';
import { ApiOperation, ApiResponse } from '@nestjs/swagger';
import { LoginDto } from './login.dto';
import { AuthService } from './auth.service';

@Controller('auth')
export class AuthController {
  private TABLA = 'auth';
  private readonly logger = new Logger(AuthController.name);

  constructor(private readonly servicio: AuthService) {}

  @Post('login')
  @ApiOperation({ summary: 'Autenticar usuario y obtener token' })
  @ApiResponse({ status: 200, description: 'Usuario autenticado con éxito.' })
  @ApiResponse({ status: 400, description: 'Solicitud incorrecta.' })
  @ApiResponse({ status: 401, description: 'Credenciales inválidas.' })
  async login(
    @Body() loginDto: LoginDto,
    @Headers('x-transaction-id') transactionId?: string,
  ) {
    this.logger.log(
      `Transaction ID: ${transactionId} - Iniciando sesión en ${this.TABLA}`,
    );
    try {
      const token = await this.servicio.login(loginDto);
      if (!token) {
        throw new HttpException(
          'Credenciales inválidas',
          HttpStatus.UNAUTHORIZED,
        );
      }
      return { message: 'Login exitoso', token };
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${transactionId} - Error al iniciar sesión en ${this.TABLA}`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        error.message || 'Error en la autenticación',
        HttpStatus.INTERNAL_SERVER_ERROR,
      );
    }
  }

  @Post('refresh')
  @ApiOperation({ summary: 'Refrescar el token de acceso' })
  @ApiResponse({ status: 200, description: 'Token refrescado con éxito.' })
  @ApiResponse({ status: 400, description: 'Solicitud incorrecta.' })
  @ApiResponse({ status: 401, description: 'Token inválido o expirado.' })
  async refresh(
    @Body() body: { refreshToken: string; dominio: string },
    @Headers('x-transaction-id') transactionId?: string,
  ) {
    this.logger.log(
      `Transaction ID: ${transactionId} - Intentando refrescar token para realm: ${body.dominio}`,
    );
    try {
      const newToken = await this.servicio.refreshToken(
        body.refreshToken,
        body.dominio,
      );
      return { message: 'Token refrescado', token: newToken };
    } catch (error) {
      this.logger.error(
        `Transaction ID: ${transactionId} - Error refrescando token`,
        error instanceof Error ? error.stack : error,
      );
      throw new HttpException(
        error.message || 'Error en la renovación del token',
        HttpStatus.UNAUTHORIZED,
      );
    }
  }
}
