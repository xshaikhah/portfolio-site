"use client";

import { useState } from 'react';
import { projectsData } from '../data/projects';

const Portfolio = ({ active }) => {
  const [activeFilter, setActiveFilter] = useState('All');
  const [isSelectOpen, setIsSelectOpen] = useState(false);

  const filters = ['All', 'AI', 'Knowledge Representation', 'Numerical Analysis', 'ML', 'Research methods', 'Summary Post', 'Intelligent Agents'];

  const handleFilterClick = (filter) => {
    setActiveFilter(filter);
    setIsSelectOpen(false);
  };

  return (
    <article className={`portfolio ${active ? 'active' : ''}`} data-page="portfolio" id="portfolio">
      <header>
        <h2 className="h2 article-title">Portfolio</h2>
      </header>

      <section className="projects">
        <ul className="filter-list">
          {filters.map(filter => (
            <li className="filter-item" key={filter}>
              <button 
                className={activeFilter === filter ? 'active' : ''} 
                onClick={() => handleFilterClick(filter)}
              >
                {filter}
              </button>
            </li>
          ))}
        </ul>

        <div className={`filter-select-box ${isSelectOpen ? 'active' : ''}`}>
          <button className="filter-select" data-select onClick={() => setIsSelectOpen(!isSelectOpen)}>
            <div className="select-value">{activeFilter}</div>
            <div className="select-icon">
              <ion-icon name="chevron-down"></ion-icon>
            </div>
          </button>

          <ul className="select-list">
            {filters.map(filter => (
              <li className="select-item" key={filter}>
                <button onClick={() => handleFilterClick(filter)}>{filter}</button>
              </li>
            ))}
          </ul>
        </div>

        <ul className="project-list">
          {projectsData.map((project, index) => (
            <li 
              className={`project-item ${activeFilter === 'All' || activeFilter === project.dataCategory ? 'active' : ''}`}
              data-filter-item 
              data-category={project.dataCategory} 
              key={index}
            >
              <a href={project.href} target="_blank" rel="noopener noreferrer">
                <figure className="project-img">
                  <div className="project-item-icon-box">
                    <ion-icon name="eye-outline"></ion-icon>
                  </div>
                  <img src={project.imgSrc} alt={project.imgAlt} loading="lazy" />
                </figure>
                <h3 className="project-title">{project.title}</h3>
                <p className="project-category">{project.category}</p>
              </a>
            </li>
          ))}
        </ul>
      </section>
    </article>
  );
};

export default Portfolio;

