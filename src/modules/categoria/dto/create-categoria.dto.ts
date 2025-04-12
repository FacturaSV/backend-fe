import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';
import { IsOptional, IsString, IsNumber } from 'class-validator';

export class CreateCategoriaDto {
  @ApiProperty({
    description: 'Nombre de la categoría',
    example: 'Lácteos',
  })
  @IsString()
  nombre: string;

  @ApiPropertyOptional({
    description: 'Descripción de la categoría',
    example: 'Productos derivados de la leche',
  })
  @IsOptional()
  @IsString()
  descripcion?: string;

  @ApiPropertyOptional({
    description: 'ID de la categoría padre (para subcategorías)',
    example: 1,
  })
  @IsOptional()
  @IsNumber()
  categoriaPadreId?: number;
}
