import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';
import { IsOptional, IsString } from 'class-validator';

// Definir el Enum en TypeScript
export enum EstadoRegistro {
  ACTIVO = 'ACTIVO',
  INACTIVO = 'INACTIVO',
  ELIMINADO = 'ELIMINADO',
}

export class CreateSucursalDto {
  @ApiProperty({
    example: 'Sucursal Central',
    description: 'El nombre de la sucursal',
  })
  @IsString()
  nombre: string;

  @ApiPropertyOptional({
    example: 'Av. Principal 123',
    description: 'La dirección de la sucursal',
  })
  @IsOptional()
  @IsString()
  direccion?: string;

  @ApiPropertyOptional({
    example: '+502 1234-5678',
    description: 'El número de teléfono de la sucursal',
  })
  @IsOptional()
  @IsString()
  telefono?: string;

  @ApiPropertyOptional({
    example: 'contacto@sucursal.com',
    description: 'El correo electrónico de la sucursal',
  })
  @IsOptional()
  @IsString()
  email?: string;

  // @ApiPropertyOptional({
  //   example: EstadoRegistro.ACTIVO,
  //   description: 'Estado del registro',
  // })
  // @IsOptional()
  // @IsEnum(EstadoRegistro)
  // estadoRt?: EstadoRegistro;
}
