COMPOSE_FILE=docker-compose.yml

help:
	@echo "Available commands:"
	@echo "  make build        - Build Docker services"
	@echo "  make up           - Start services"
	@echo "  make down         - Stop services"
	@echo "  make restart      - Restart services"
	@echo "  make logs         - View application logs"
	@echo "  make clean        - Remove containers and networks"

build:
	docker-compose -f $(COMPOSE_FILE) build

up:
	docker-compose -f $(COMPOSE_FILE) up -d

down:
	docker-compose -f $(COMPOSE_FILE) down

restart:
	docker-compose -f $(COMPOSE_FILE) down
	docker-compose -f $(COMPOSE_FILE) up -d

logs:
	docker-compose -f $(COMPOSE_FILE) logs -f erlang_app

clean:
	docker-compose -f $(COMPOSE_FILE) down --volumes --remove-orphans
