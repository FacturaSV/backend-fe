import { PartialType } from '@nestjs/mapped-types';
import { ApiPropertyOptional } from '@nestjs/swagger';
import { CreateSucursalDto } from './create-sucursal.dto';
import { IsEnum, IsOptional, IsNumber } from 'class-validator';
import { EstadoRegistro } from './create-sucursal.dto';

export class UpdateSucursalDto extends PartialType(CreateSucursalDto) {
  @ApiPropertyOptional({
    example: 1,
    description: 'ID del usuario que actualiza la sucursal',
  })
  @IsOptional()
  @IsNumber()
  updatedBy?: number;

  @ApiPropertyOptional({
    example: EstadoRegistro.ACTIVO,
    description: 'Estado del registro',
  })
  @IsOptional()
  @IsEnum(EstadoRegistro)
  estado?: EstadoRegistro;
}
