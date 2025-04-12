// src/modules/producto-categoria/dto/update-producto-categoria.dto.ts
import { IsOptional } from 'class-validator';
import { EstadoRegistro } from '@prisma/client';

export class UpdateProductoCategoriaDto {
  @IsOptional()
  productoId?: number;

  @IsOptional()
  categoriaId?: number;

  @IsOptional()
  estadoRt?: EstadoRegistro;

  // @IsOptional()
  // updatedBy?: number;
}
