import { Injectable, Logger } from '@nestjs/common';
import { Cron, Timeout } from '@nestjs/schedule';
import { PrismaGlobalService } from 'src/prisma/prisma-global.service';
import { CronJobAuthService } from './cron-job-auth.service';
import { CronJobTrxDteService } from './cron-job-trx-dte.service';

@Injectable()
export class CronJobService {
  private readonly logger = new Logger(CronJobService.name);

  constructor(
    private readonly prisma: PrismaGlobalService,
    private readonly auth: CronJobAuthService,
    private readonly TrxDte: CronJobTrxDteService,
  ) {}

  // @Cron('*/1 * * * *')
  // async handleCronEmail() {
  //   this.logger.log(
  //     `Ejecutando tarea programada para enviar correos electrónicos...`,
  //   );
  //   this.logger.log(`Se ejecuta cada 3 minutos...`);
  //   const result = await this.TrxDte.buscarDinamicoTrx({
  //     AND: [],
  //     OR: [
  //       {
  //         columna: 'estadoTranDTE',
  //         operador: '=',
  //         valor: 'PROCESADO',
  //       },
  //     ],
  //   });
  //   this.logger.log('buscarDinamicoTrx', result);
  // }

  @Cron('25 3 * * *')
  async handleCron() {
    const users = await this.prisma.credenciales.findMany();
    for (const user of users) {
      const url = user.uriAuth
        ? user.uriAuth
        : process.env.AUTH_HACIENDA_URL || '';

      const token = await this.auth.obtenerToken(
        url,
        user.usuario,
        user.password,
      );
      user.accessToken = token.body.token;
      await this.prisma.credenciales.update({
        where: { id: user.id },
        data: user,
      });
      this.logger.log(
        `Generacion de token ejecutado correctamente para el usuario: ${user.nombreCliente}`,
      );
    }
  }

  @Timeout(5000000)
  async handleTimeout() {
    const users = await this.prisma.credenciales.findMany();
    for (const user of users) {
      const url = user.uriAuth
        ? user.uriAuth
        : process.env.AUTH_HACIENDA_URL || '';

      const token = await this.auth.obtenerToken(
        url,
        user.usuario,
        user.password,
      );
      user.accessToken = token.body.token;
      await this.prisma.credenciales.update({
        where: { id: user.id },
        data: user,
      });
      this.logger.log(
        `Generacion de token ejecutado correctamente para el usuario: ${user.nombreCliente}`,
      );
    }
  }
}
