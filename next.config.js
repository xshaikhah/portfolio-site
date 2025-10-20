/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  // The basePath is essential for GitHub Pages since the site is in a subdirectory.
  basePath: '/portfolio-site',
  // Set assetPrefix to match GitHub Pages URL structure
  assetPrefix: '/portfolio-site/',
  images: {
    unoptimized: true,
  },
};

module.exports = nextConfig;
