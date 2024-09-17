# Usar una imagen base de Erlang
FROM erlang:26

# Establecer el directorio de trabajo dentro del contenedor
WORKDIR /app

# Copiar el archivo rebar.config y el código fuente al contenedor
COPY rebar.config rebar.lock /app/
COPY apps /app/apps

# Instalar las dependencias y compilar el proyecto
RUN rebar3 compile

# Exponer el puerto en el que la aplicación escucha
EXPOSE 8080

# Comando para ejecutar la aplicación
CMD ["rebar3", "shell"]