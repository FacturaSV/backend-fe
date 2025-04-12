import { Global, Module } from '@nestjs/common';
import { PrismaService } from './prisma.service';
import { PrismaGlobalService } from './prisma-global.service';

@Global()
@Module({
  providers: [PrismaService, PrismaGlobalService],
  exports: [PrismaService, PrismaGlobalService],
})
export class PrismaModule {}
