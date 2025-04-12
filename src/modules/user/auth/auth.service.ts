import { Injectable, Logger } from '@nestjs/common';
import axios from 'axios';
import { LoginDto } from './login.dto';

@Injectable()
export class AuthService {
  private keycloakBaseUrl: string = process.env.KEYCLOAK_BASE_URL ?? '';
  private clientId: string = process.env.KEYCLOAK_CLIENT_ID ?? '';
  private clientSecret: string = process.env.KEYCLOAK_CLIENT_SECRET ?? '';
  private readonly logger = new Logger(AuthService.name);

  async login(loginDto: LoginDto): Promise<any> {
    const { username, password } = loginDto;
    const tokenUrl = `${this.keycloakBaseUrl}/realms/facturacion-electronica/protocol/openid-connect/token`;

    this.logger.log(
      `Intentando autenticación para usuario: ${username} en el realm: facturacion-electronica`,
    );

    const body = new URLSearchParams();
    body.append('client_id', this.clientId);
    body.append('client_secret', this.clientSecret);
    body.append('grant_type', 'password');
    body.append('username', username);
    body.append('password', password);
    body.append('scope', 'openid email profile domain-scope');

    try {
      const response = await axios.post(tokenUrl, body.toString(), {
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      });

      this.logger.log(`Autenticación exitosa para ${username}`);
      return response.data;
    } catch (error) {
      this.logger.error(
        `Error autenticando ${username}:`,
        error.response?.data || error.message,
      );
    }
  }

  async refreshToken(refreshToken: string, dominio: string): Promise<any> {
    const tokenUrl = `${this.keycloakBaseUrl}/realms/${dominio}/protocol/openid-connect/token`;

    this.logger.log(`Intentando refrescar token en el realm: ${dominio}`);

    const body = new URLSearchParams();
    body.append('client_id', this.clientId);
    body.append('client_secret', this.clientSecret);
    body.append('grant_type', 'refresh_token');
    body.append('refresh_token', refreshToken);

    try {
      const response = await axios.post(tokenUrl, body.toString(), {
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      });

      this.logger.log('🔄 Token refrescado exitosamente.');
      return response.data;
    } catch (error) {
      this.logger.error(
        `Error refrescando token:`,
        error.response?.data || error.message,
      );
      throw new Error(
        error.response?.data?.error_description || 'Error refrescando token',
      );
    }
  }
}
