import { Module } from '@nestjs/common';
import { CronJobService } from './cron-job.service';
import { ScheduleModule } from '@nestjs/schedule';
import { PrismaGlobalService } from 'src/prisma/prisma-global.service';
import { CronJobAuthService } from './cron-job-auth.service';
import { CronJobTrxDteService } from './cron-job-trx-dte.service';

@Module({
  imports: [ScheduleModule.forRoot()],
  providers: [
    CronJobService,
    CronJobAuthService,
    CronJobTrxDteService,
    PrismaGlobalService,
  ],
  exports: [CronJobService, CronJobAuthService, CronJobTrxDteService],
})
export class CronJobModule {}
