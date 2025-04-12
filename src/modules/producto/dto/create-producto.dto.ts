import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';
import { IsString, IsOptional, IsInt } from 'class-validator';
// Si quieres validar con enums, descomenta esto
// import { EstadoRegistro } from '@prisma/client';
// import { IsEnum } from 'class-validator';

export class CreateProductoDto {
  @ApiProperty({
    description: 'Código interno o SKU del producto',
    example: 'AB-123',
  })
  @IsString()
  codigo: string;

  @ApiProperty({
    description: 'Nombre del producto',
    example: 'Leche Entera',
  })
  @IsString()
  nombre: string;

  @ApiPropertyOptional({
    description: 'Descripción detallada del producto',
    example: 'Leche entera en caja de 1 litro, marca X',
  })
  @IsOptional()
  @IsString()
  descripcion?: string;

  @ApiPropertyOptional({
    description: 'Unidad de medida (p. ej. "LITRO", "KILOGRAMO", etc.)',
    example: 'LITRO',
  })
  @IsOptional()
  @IsString()
  unidadMedida?: string;

  // Si deseas permitir especificar el estado en la creación:
  /*
  @ApiPropertyOptional({
    description: 'Estado del registro (ACTIVO, INACTIVO, ELIMINADO)',
    example: 'ACTIVO',
  })
  @IsOptional()
  @IsEnum(EstadoRegistro)
  estadoRt?: EstadoRegistro;
  */

  // Si deseas que el cliente indique quién creó el registro:
  @ApiPropertyOptional({
    description: 'ID del usuario que crea el producto',
    example: 1,
  })
  @IsOptional()
  @IsInt()
  createdBy?: number;
}
