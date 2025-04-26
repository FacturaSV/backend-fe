import { Module } from '@nestjs/common';
import { CacheModule } from '@nestjs/cache-manager';
import { DteService } from './dte.service';
import { DteFeController } from './dte.fe.controller';
import { DteFeMapper } from './dte.fe.mapper';
import { TrxDteService } from './trx-dte.service';
import { DteCcfController } from './dte.ccf.controller';

@Module({
  imports: [
    CacheModule.register({
      ttl: 60 * 60 * 24, // 24 horas
      isGlobal: false,
    }),
  ],
  controllers: [DteFeController, DteCcfController],
  providers: [DteService, TrxDteService, DteFeMapper],
})
export class DteModule {}
