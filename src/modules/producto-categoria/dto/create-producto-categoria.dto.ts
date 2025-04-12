import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';
import { IsNotEmpty, IsOptional } from 'class-validator';
import { EstadoRegistro } from '@prisma/client'; // Adjust import path as needed

export class CreateProductoCategoriaDto {
  @ApiProperty({
    description: 'El ID del producto.',
    example: 123,
  })
  @IsNotEmpty()
  productoId: number;

  @ApiProperty({
    description: 'El ID de la categoría.',
    example: 456,
  })
  @IsNotEmpty()
  categoriaId: number;

  @ApiPropertyOptional({
    description: 'Estado del registro. Por defecto ACTIVO.',
    enum: EstadoRegistro,
    default: EstadoRegistro.ACTIVO,
  })
  @IsOptional()
  estadoRt?: EstadoRegistro = EstadoRegistro.ACTIVO;

  // Si deseas manejar los campos createdBy, etc., se haría algo similar:
  // @ApiPropertyOptional({
  //   description: 'Id del usuario que crea el registro',
  //   example: 1,
  // })
  // @IsOptional()
  // createdBy?: number;
}
