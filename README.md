# FEARBASE-R-Package

**Short description:**  < short description that explains what this repo is used for >

|               |        |
|---------------|--------|
| Product Owner | < email > |
| Developer     | < email > |
| dev url       | < xxxxx > |
| prod url      | < xxxxx > |
| Language      | R      |
| ...           |        |
| License       |        |

## further optional information


### Following R packages are used: 
...

# Development

## Testing with a local OpenCPU server

You can test how any changes you make to the fearbase R package affect the deployment of the OpenCPU server by running an
OpenCPU server locally.


### Start the OpenCPU server:

In the `dev_local` directory of this repository run:

```bash
docker compose up -d --build
```

### Stop the OpenCPU server

```php
docker compose down
```

### Update the OpenCPU server 
After making changes to the fearbase package. Simply run
```php
docker compose up -d --build
```
again.
If you **don't see any changes** it might be the browser caching an old state of the Docker container.
In that case either delete your browser cache or open the OpenCPU API Explorer in a new **private/icognito session**.

### Testing on the OpenCPU server

Open [http://localhost/ocpu/](http://localhost:80/ocpu/) in your browser to access the OpenCPU API Explorer interface.

In the interface you can run any functions and access included data of the fearbase package by making requests in `../library/fearbase/`. 
Check the [OpenCPU API docs](https://www.opencpu.org/api.html#api-methods) for information on how to use the OpenCPU API Explorer