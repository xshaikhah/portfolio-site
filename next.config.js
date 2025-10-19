const isProd = process.env.NODE_ENV === 'production';

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  basePath: isProd ? '/portfolio-site' : '',
  assetPrefix: isProd ? '/portfolio-site' : '',
  images: {
    unoptimized: true,
  },
};

module.exports = nextConfig;
