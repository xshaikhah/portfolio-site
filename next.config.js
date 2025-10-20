/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  // The basePath is essential for GitHub Pages since the site is in a subdirectory.
  basePath: '/portfolio-site',
  // The assetPrefix is also needed for the same reason.
  assetPrefix: '/portfolio-site',
  images: {
    unoptimized: true,
  },
};

module.exports = nextConfig;
