import Script from 'next/script';

const isProd = process.env.NODE_ENV === 'production';
const assetPrefix = isProd ? '/portfolio-site' : '';

export const metadata = {
  title: 'Shaikha Alharthi - Academic Portfolio',
};

export default function RootLayout({ children }) {
  return (
    <html lang="en" suppressHydrationWarning>
      <head>
        <link rel="shortcut icon" href={`${assetPrefix}/assets/images/logo.ico`} type="image/x-icon" />

        {/* Custom CSS link */}
        <link rel="stylesheet" href={`${assetPrefix}/assets/css/style.css`} />
        <link rel="stylesheet" href={`${assetPrefix}/assets/css/custom.css`} />

        {/* Google font link */}
        <link rel="preconnect" href="https://fonts.googleapis.com" />
        <link rel="preconnect" href="https://fonts.gstatic.com" crossOrigin="anonymous" />
        <link
          href="https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600&display=swap"
          rel="stylesheet"
        />
      </head>
      <body>
        {children}

        <Script type="module" src="https://unpkg.com/ionicons@5.5.2/dist/ionicons/ionicons.esm.js" crossOrigin="anonymous"></Script>
        <Script nomodule src="https://unpkg.com/ionicons@5.5.2/dist/ionicons/ionicons.js" crossOrigin="anonymous"></Script>
      </body>
    </html>
  );
}
