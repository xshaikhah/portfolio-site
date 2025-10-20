"use client";

import { Suspense } from 'react';
import { useSearchParams } from 'next/navigation';
import Link from 'next/link';
import Sidebar from '../components/Sidebar';
import Portfolio from '../components/Portfolio';
import Contact from '../components/Contact';

function PageContent() {
  const searchParams = useSearchParams();
  const activePage = searchParams.get('page') || 'about';

  return (
    <main>
      <Sidebar />

      <div className="main-content">
        <nav className="navbar">
          <ul className="navbar-list">
            <li className="navbar-item">
              <Link href="/?page=about" className={`navbar-link ${activePage === 'about' ? 'active' : ''}`}>About</Link>
            </li>
            <li className="navbar-item">
              <Link href="/?page=portfolio" className={`navbar-link ${activePage === 'portfolio' ? 'active' : ''}`}>Portfolio</Link>
            </li>
            <li className="navbar-item">
              <Link href="/?page=contact" className={`navbar-link ${activePage === 'contact' ? 'active' : ''}`}>Contact</Link>
            </li>
          </ul>
        </nav>

        <article className={`about ${activePage === 'about' ? 'active' : ''}`} data-page="about">
          <header>
            <h2 className="h2 article-title">About me</h2>
          </header>

          <section className="about-text">
            <p>
              I'm Shaikha Alharthi, a Computer Science student at Essex University specializing in AI and Data Science. I'm passionate about leveraging computational methods to solve complex problems and create intelligent systems.
            </p>
            <p>
              My academic journey focuses on the intersection of mathematics and computer science, with particular emphasis on machine learning algorithms, knowledge representation systems, and numerical analysis. I enjoy exploring how AI can transform data into meaningful insights and create solutions that have real-world impact.
            </p>
            <p>
              Through my coursework and projects, I've developed skills in algorithm design, data analysis, and computational modeling. I'm particularly interested in how these technologies can be applied to challenges in fields like healthcare, environmental science, and business analytics.
            </p>
          </section>

          <section className="service">
            <h3 className="h3 service-title">My Academic Modules</h3>
            <ul className="service-list">
              <li className="service-item">
                <div className="service-icon-box">
                  <img src="/portfolio-site/assets/images/icon-design.svg" alt="AI icon" width="40" />
                </div>
                <div className="service-content-box">
                  <h4 className="h4 service-item-title">AI</h4>
                  <p className="service-item-text">
                    Study of intelligent agents and systems that can perceive their environment and take actions to achieve goals.
                  </p>
                </div>
              </li>
              <li className="service-item">
                <div className="service-icon-box">
                  <img src="/portfolio-site/assets/images/icon-dev.svg" alt="Knowledge icon" width="40" />
                </div>
                <div className="service-content-box">
                  <h4 className="h4 service-item-title">Knowledge Representation and Reasoning</h4>
                  <p className="service-item-text">
                    Formal representation of information and logical reasoning to solve complex problems.
                  </p>
                </div>
              </li>
              <li className="service-item">
                <div className="service-icon-box">
                  <img src="/portfolio-site/assets/images/icon-app.svg" alt="ML icon" width="40" />
                </div>
                <div className="service-content-box">
                  <h4 className="h4 service-item-title">Machine Learning</h4>
                  <p className="service-item-text">
                    Development of algorithms and statistical models that enable computers to learn from and make predictions on data.
                  </p>
                </div>
              </li>
              <li className="service-item">
                <div className="service-icon-box">
                  <img src="/portfolio-site/assets/images/icon-numerical.svg" alt="numerical icon" width="40" />
                </div>
                <div className="service-content-box">
                  <h4 className="h4 service-item-title">Numerical Analysis</h4>
                  <p className="service-item-text">
                    Study of algorithms that use numerical approximation for problems of mathematical analysis.
                  </p>
                </div>
              </li>
              <li className="service-item">
                <div className="service-icon-box">
                  <img src="/portfolio-site/assets/images/icon-dev.svg" alt="research methods icon" width="40" />
                </div>
                <div className="service-content-box">
                  <h4 className="h4 service-item-title">Research methods and professional practice</h4>
                  <p className="service-item-text">
                    Developing critical skills for conducting and evaluating research, alongside professional ethics and practices in computer science.
                  </p>
                </div>
              </li>
              <li className="service-item">
                <div className="service-icon-box">
                  <img src="/portfolio-site/assets/images/icon-summary.svg" alt="summary icon" width="40" />
                </div>
                <div className="service-content-box">
                  <h4 className="h4 service-item-title">Summary Post</h4>
                  <p className="service-item-text">
                    Comprehensive analysis and synthesis of research findings and academic concepts.
                  </p>
                </div>
              </li>
            </ul>
          </section>
        </article>

        <Suspense fallback={<div>Loading...</div>}>
          <Portfolio active={activePage === 'portfolio'} />
        </Suspense>

        <Contact active={activePage === 'contact'} />
      </div>
    </main>
  );
}

export default function Page() {
  return (
    <Suspense fallback={<div>Loading...</div>}>
      <PageContent />
    </Suspense>
  );
}
