import { Module } from '@nestjs/common';
import { HistorialTablasService } from './historial-tablas.service';

@Module({
  providers: [HistorialTablasService],
})
export class HistorialTablasModule {}
