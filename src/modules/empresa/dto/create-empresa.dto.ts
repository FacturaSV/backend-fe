import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';
import {
  IsString,
  IsOptional,
  IsInt,
  IsUrl,
  IsEnum,
  IsEmail,
} from 'class-validator';
import { EstadoRegistro } from '@prisma/client';

export class CreateEmpresaDto {
  @ApiProperty({
    description: 'Nombre de la empresa',
    example: 'Tech Solutions S.A.',
  })
  @IsString()
  nombre: string;

  @ApiPropertyOptional({
    description: 'URL del logo de la empresa',
    example: 'https://example.com/logo.png',
  })
  @IsOptional()
  @IsUrl()
  logo?: string;

  @ApiPropertyOptional({
    description: 'Dirección de la empresa',
    example: 'Av. Siempre Viva 123',
  })
  @IsOptional()
  @IsString()
  direccion?: string;

  @ApiPropertyOptional({
    description: 'Número de Identificación Tributaria (NIT)',
    example: '12345678901234',
  })
  @IsOptional()
  @IsString()
  nit?: string;

  @ApiPropertyOptional({
    description: 'Número de Registro de Contribuyente (NRC)',
    example: '987654-3',
  })
  @IsOptional()
  @IsString()
  nrc?: string;

  @ApiPropertyOptional({
    description: 'Teléfono de la empresa',
    example: '+503 2256-7890',
  })
  @IsOptional()
  @IsString()
  telefono?: string;

  @ApiPropertyOptional({
    description: 'Actividad económica de la empresa',
    example: 'Servicios de tecnología',
  })
  @IsOptional()
  @IsString()
  actividadEconomica?: string;

  @ApiPropertyOptional({
    description: 'Correo electrónico de la empresa',
    example: 'contacto@empresa.com',
  })
  @IsOptional()
  @IsEmail()
  correo?: string;

  @ApiPropertyOptional({
    description: 'ID del grupo en Keycloak asociado a la empresa',
    example: '123e4567-e89b-12d3-a456-426614174000',
  })
  @IsOptional()
  @IsString()
  keycloakGroupId?: string;

  @ApiProperty({
    description: 'Cantidad de facturas permitidas',
    example: 100,
  })
  @IsOptional()
  @IsInt()
  facturasPermitidas?: number;

  @ApiPropertyOptional({
    description: 'Cantidad de facturas ya utilizadas',
    example: 10,
  })
  @IsOptional()
  @IsInt()
  facturasUtilizadas?: number;

  @ApiPropertyOptional({
    description: 'Cantidad de usuarios administradores permitidos',
    example: 2,
  })
  @IsOptional()
  @IsInt()
  userAdminCant?: number;

  @ApiPropertyOptional({
    description: 'Cantidad de usuarios dedicados permitidos',
    example: 5,
  })
  @IsOptional()
  @IsInt()
  userDedicadosCant?: number;

  @ApiPropertyOptional({
    description: 'Estado de la empresa (ACTIVO, INACTIVO, ELIMINADO)',
    example: 'ACTIVO',
  })
  @IsOptional()
  @IsEnum(EstadoRegistro)
  estadoRt?: EstadoRegistro;
}
