# Usamos Node.js 20.11.1
FROM node:20.11.1-alpine AS builder

# Configuración del directorio de trabajo
WORKDIR /app

# Copiamos los archivos de configuración
COPY package*.json prisma/schema.prisma ./

# Instalamos TODAS las dependencias (incluyendo las de desarrollo)
RUN npm install

# Generamos los tipos de Prisma
RUN npx prisma generate

# Copiamos el resto del código
COPY . .

# Compilamos el código TypeScript y verificamos que `dist/` se crea
RUN npx nest build && ls -lah dist

# --------------------
# Imagen final
# --------------------
FROM node:20.11.1-alpine AS runner

# Configuración del directorio de trabajo
WORKDIR /app

# Copiamos las dependencias y el código compilado
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/dist ./dist

# Exponemos el puerto del backend
EXPOSE 3000

# Comando de inicio
CMD ["node", "dist/main.js"]
