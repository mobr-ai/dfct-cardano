FROM python:3.11-slim

# Install required system packages
RUN apt-get update && apt-get install -y \
    build-essential curl git libgmp-dev libffi-dev libssl-dev libpq-dev && \
    rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Install Poetry
RUN curl -sSL https://install.python-poetry.org | python3 - && \
    ln -s /root/.local/bin/poetry /usr/local/bin/poetry

# Copy project files
COPY . /app

# Install dependencies using Poetry
RUN poetry config virtualenvs.create false && \
    poetry install --no-root --no-interaction --no-ansi

# Expose FastAPI default port
EXPOSE 8000

# Command passed via docker-compose
CMD ["uvicorn", "dfctbackend.main:app", "--host", "0.0.0.0", "--port", "8000"]

