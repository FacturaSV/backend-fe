import { Module } from '@nestjs/common';
import { ProductoCategoriaService } from './producto-categoria.service';
import { ProductoCategoriaController } from './producto-categoria.controller';

@Module({
  controllers: [ProductoCategoriaController],
  providers: [ProductoCategoriaService],
})
export class ProductoCategoriaModule {}
