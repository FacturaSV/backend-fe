import {
  Injectable,
  Logger,
  OnModuleInit,
  OnModuleDestroy,
} from '@nestjs/common';
import { PrismaClient } from '@prisma/client';

@Injectable()
export class PrismaGlobalService
  extends PrismaClient
  implements OnModuleInit, OnModuleDestroy
{
  private readonly logger = new Logger(PrismaGlobalService.name);

  async onModuleInit() {
    await this.$connect();
    this.logger.log('Prisma Global Service connected');
  }

  async onModuleDestroy() {
    await this.$disconnect();
    this.logger.log('Prisma Global Service disconnected');
  }
}
