import { ApiProperty, ApiPropertyOptional } from '@nestjs/swagger';
import { IsEmail, IsOptional, IsString } from 'class-validator';

export class CreateUserDto {
  id?: number;
  @ApiProperty({
    example: 'usuario@email.com',
    description: 'Correo electrónico único del usuario',
  })
  @IsEmail()
  email: string;

  @ApiPropertyOptional({
    example: 'usuario123',
    description: 'Nombre de usuario (opcional)',
  })
  @IsOptional()
  @IsString()
  username?: string;

  @ApiPropertyOptional({
    example: 'Juan',
    description: 'Nombre del usuario',
  })
  @IsOptional()
  @IsString()
  nombre?: string;

  @ApiPropertyOptional({
    example: 'Pérez',
    description: 'Apellidos del usuario',
  })
  @IsOptional()
  @IsString()
  apellidos?: string;
}
