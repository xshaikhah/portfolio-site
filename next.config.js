/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  // Always use basePath and assetPrefix
  basePath: '/portfolio-site',
  assetPrefix: '/portfolio-site/',
  images: {
    unoptimized: true,
  },
};

module.exports = nextConfig;
