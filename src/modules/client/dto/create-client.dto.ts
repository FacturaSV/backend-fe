import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';
import { IsString, IsOptional, IsInt, IsEmail, IsEnum } from 'class-validator';
import { EstadoRegistro } from '@prisma/client';

export class CreateClientDto {
  @ApiProperty({ description: 'Nombre del cliente', example: 'Juan Pérez' })
  @IsString()
  nombre: string;

  @ApiProperty({
    description: 'Correo electrónico del cliente',
    example: 'juan.perez@example.com',
  })
  @IsEmail()
  email: string;

  @ApiPropertyOptional({
    description: 'Número de teléfono',
    example: '+521234567890',
  })
  @IsOptional()
  @IsString()
  telefono?: string;

  @ApiPropertyOptional({
    description: 'ID de la empresa a la que pertenece',
    example: 1,
  })
  @IsOptional()
  @IsInt()
  empresaId?: number;

  @ApiPropertyOptional({
    description: 'Estado del cliente (ACTIVO, INACTIVO, ELIMINADO)',
    example: 'ACTIVO',
  })
  @IsOptional()
  @IsEnum(EstadoRegistro)
  estadoRt?: EstadoRegistro;
}
