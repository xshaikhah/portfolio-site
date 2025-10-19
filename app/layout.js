import Script from 'next/script';

export const metadata = {
  title: 'Shaikha Alharthi - Academic Portfolio',
};

export default function RootLayout({ children }) {
  return (
    <html lang="en" suppressHydrationWarning>
      <head>
        <link rel="shortcut icon" href="/assets/images/logo.ico" type="image/x-icon" />

        {/* Custom CSS link */}
        <link rel="stylesheet" href="/assets/css/style.css" />
        <link rel="stylesheet" href="/assets/css/custom.css" />

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
