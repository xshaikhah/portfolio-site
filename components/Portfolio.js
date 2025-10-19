"use client";

import { useState } from 'react';
import { useRouter, usePathname, useSearchParams } from 'next/navigation';
import { projectsData } from '../data/projects';
import ProjectModal from './ProjectModal';
import DiscussionModal from './DiscussionModal';

const Portfolio = ({ active }) => {
  const router = useRouter();
  const pathname = usePathname();
  const searchParams = useSearchParams();
  const activeFilter = searchParams.get('filter') || 'All';

  const [isSelectOpen, setIsSelectOpen] = useState(false);
  const [selectedProject, setSelectedProject] = useState(null);
  const [isDiscussionModalOpen, setIsDiscussionModalOpen] = useState(false);

  const filters = ['All', 'AI', 'Knowledge Representation', 'Numerical Analysis', 'ML', 'Research methods', 'Summary Post', 'Intelligent Agents'];

  const handleFilterClick = (filter) => {
    const params = new URLSearchParams(searchParams);
    params.set('filter', filter);
    router.push(`${pathname}?${params.toString()}`);
    setIsSelectOpen(false);
  };

  const handleProjectClick = (e, project) => {
    if (project.title === 'Project Report - Academic research agent') {
      e.preventDefault();
      setSelectedProject(project);
    } else if (project.title === 'Agent Communication Languages - Discussion') {
      e.preventDefault();
      setIsDiscussionModalOpen(true);
    }
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
          {selectedProject && <ProjectModal project={selectedProject} onClose={() => setSelectedProject(null)} />}
          {isDiscussionModalOpen && <DiscussionModal onClose={() => setIsDiscussionModalOpen(false)} />}
          {projectsData.map((project, index) => (
            <li 
              className={`project-item ${activeFilter === 'All' || activeFilter === project.dataCategory ? 'active' : ''}`}
              data-filter-item 
              data-category={project.dataCategory} 
              key={index}
            >
              <a href={project.href} target="_blank" rel="noopener noreferrer" onClick={(e) => handleProjectClick(e, project)}>
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

