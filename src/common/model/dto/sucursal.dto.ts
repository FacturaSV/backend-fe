import { IsOptional, IsString, IsBoolean, IsNumber } from 'class-validator';
import { Type } from 'class-transformer';
import { PartialType } from '@nestjs/mapped-types';

export class CreateSucursalDto {
  @IsString()
  nombre: string;

  @IsOptional()
  @IsString()
  direccion?: string;

  @IsOptional()
  @IsString()
  telefono?: string;

  @IsOptional()
  @IsString()
  email?: string;

  @IsOptional()
  @IsBoolean()
  activo?: boolean;
}

export class UpdateSucursalDto extends PartialType(CreateSucursalDto) {}

export class PaginationDto {
  @IsOptional()
  @Type(() => Number)
  @IsNumber()
  page?: number;

  @IsOptional()
  @Type(() => Number)
  @IsNumber()
  limit?: number;
}

export class SearchSucursalDto extends PaginationDto {
  @IsOptional()
  @IsString()
  term?: string; // Término de búsqueda (nombre, dirección, teléfono, email)

  @IsOptional()
  @IsBoolean()
  isActive?: boolean; // Filtrar por estado activo/inactivo
}
