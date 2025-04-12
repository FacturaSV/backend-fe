import { ApiProperty } from '@nestjs/swagger';
import { IsNotEmpty } from 'class-validator';

export class LoginDto {
  // @ApiProperty({
  //   description: 'El dominio o realm de autenticación en Keycloak.',
  //   example: 'facturacion-electronica',
  // })
  // @IsNotEmpty({ message: 'El dominio es obligatorio.' })
  // dominio: string;

  @ApiProperty({
    description: 'El nombre de usuario para autenticación.',
    example: 'usuario123',
  })
  @IsNotEmpty({ message: 'El nombre de usuario es obligatorio.' })
  username: string;

  @ApiProperty({
    description: 'La contraseña del usuario.',
    example: 'secreto123',
  })
  @IsNotEmpty({ message: 'La contraseña es obligatoria.' })
  password: string;
}
