import { Injectable, Logger } from '@nestjs/common';
import axios from 'axios';

interface AuthResponse {
  status: string;
  body: {
    user: string;
    token: string;
    rol: {
      nombre: string;
      codigo: string;
      descripcion: string | null;
      rolSuperior: string | null;
      nivel: number | null;
      activo: boolean | null;
      permisos: string[] | null;
    };
    roles: string[];
    tokenType: string;
  };
}

@Injectable()
export class CronJobAuthService {
  private readonly logger = new Logger(CronJobAuthService.name);

  async obtenerToken(
    uri: string,
    user: string,
    pass: string,
  ): Promise<AuthResponse> {
    try {
      const response = await axios.post(
        uri,
        new URLSearchParams({
          user: user,
          pwd: pass,
        }).toString(),
        {
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
        },
      );

      this.logger.log(
        `Respuesta del servicio auth generacion token 24 horas: ${JSON.stringify(response.status)}`,
      );
      return response.data; // Puedes retornar el token o lo que necesites
    } catch (error) {
      this.logger.error(`Error al obtener el token: ${error.message}`);
      throw error;
    }
  }
}
