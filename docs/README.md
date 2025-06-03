# Velang Documentation Server

## Overview

The `server.ve` file implements a lightweight HTTP server for serving Velang documentation. It's written in Velang itself and demonstrates the language's capabilities for systems programming.

## Features

- ✅ **Clean URLs**: Serves `/getting-started` instead of `/getting-started.html`
- ✅ **Version Replacement**: Automatically replaces `{{VERSION}}` with version from `Cargo.toml`
- ✅ **Custom 404 Page**: Beautiful error page with navigation back to home
- ✅ **Static File Serving**: Serves HTML documentation files
- ✅ **Low Resource Usage**: Minimal memory footprint and fast startup
- ✅ **Windows Compatible**: Built using Windows API (WinSock2)

## Local Development

### Prerequisites

1. Rust installed (for building Velang compiler)
2. Velang compiler built: `cargo build --release`

### Running Locally

```bash
# Compile the server
./target/release/ve ./docs/server.ve

# Run the server
./docs/build/program.exe
```

The server will start on `http://localhost:8080`

### Testing

```bash
# Test homepage
curl http://localhost:8080/

# Test clean URLs
curl http://localhost:8080/getting-started

# Test 404 page
curl http://localhost:8080/nonexistent-page
```

## Deployment

### Option 1: GitHub Actions (Recommended)

The repository includes a GitHub Action (`.github/workflows/deploy-docs.yml`) that:

1. Builds the Velang compiler
2. Compiles the documentation server
3. Deploys it as a systemd service
4. Sets up optional nginx reverse proxy
5. Runs health checks

### Option 2: Docker

```bash
# Build the Docker image
docker build -f Dockerfile.docs -t velang-docs .

# Run the container
docker run -p 8080:8080 velang-docs
```

### Option 3: Manual Deployment

1. **Build the compiler:**
   ```bash
   cargo build --release
   ```

2. **Compile the server:**
   ```bash
   ./target/release/ve ./docs/server.ve
   ```

3. **Deploy the binary:**
   ```bash
   # Copy to server
   scp ./docs/build/program.exe user@server:/opt/velang-docs/
   scp -r ./docs/*.html user@server:/opt/velang-docs/docs/
   scp ./Cargo.toml user@server:/opt/velang-docs/
   ```

4. **Create systemd service:**
   ```bash
   sudo tee /etc/systemd/system/velang-docs.service > /dev/null <<EOF
   [Unit]
   Description=Velang Documentation Server
   After=network.target
   
   [Service]
   Type=simple
   User=www-data
   WorkingDirectory=/opt/velang-docs
   ExecStart=/opt/velang-docs/program.exe
   Restart=always
   RestartSec=10
   
   [Install]
   WantedBy=multi-user.target
   EOF
   ```

5. **Start the service:**
   ```bash
   sudo systemctl daemon-reload
   sudo systemctl enable velang-docs
   sudo systemctl start velang-docs
   ```

## Security Considerations

- The server binds to all interfaces (`0.0.0.0:8080`)
- No authentication or rate limiting implemented
- Consider running behind a reverse proxy (nginx/apache) for production
- File paths are sanitized to prevent directory traversal

## Performance

- **Memory Usage**: ~1-2 MB
- **Startup Time**: <100ms  
- **Request Handling**: Single-threaded, handles one request at a time
- **File Caching**: Files are read from disk on each request

## Troubleshooting

### Server won't start
- Check if port 8080 is available: `netstat -an | findstr 8080`
- Verify all required files exist in `docs/` directory
- Check `Cargo.toml` exists in project root

### 404 errors for existing pages
- Ensure HTML files are in `docs/` directory
- Check file permissions
- Verify clean URLs don't include `.html` extension

### Version not showing
- Verify `Cargo.toml` exists and contains `version = "x.x.x"`
- Check that `{{VERSION}}` placeholder exists in `index.html`

## Contributing

The server code is intentionally minimal to demonstrate Velang's capabilities. Improvements welcome:

- Add HTTPS support
- Implement request logging
- Add configuration file support
- Multi-threading support
- Static file caching

## License

Same as the main Velang project.
