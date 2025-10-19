"use client";

const ProjectModal = ({ project, onClose }) => {
  if (!project) return null;

  return (
    <div className="modal-container active">
      <div className="overlay active" onClick={onClose}></div>
      <div className="testimonials-modal">
        <button className="modal-close-btn" onClick={onClose}>
          <ion-icon name="close-outline"></ion-icon>
        </button>

        <div className="modal-content">
          <h4 className="h3 modal-title">{project.title}</h4>
          <div className="project-details">
            <a href={project.href} target="_blank" rel="noopener noreferrer" className="form-btn" style={{ marginBottom: '30px' }}>
              <ion-icon name="document-text-outline"></ion-icon>
              <span>View Full Report</span>
            </a>
            <div className="uml-diagrams">
              <div className="uml-item">
                <h5>UML Diagram</h5>
                <img src="/images/UML diagram.png" alt="UML Diagram" />
              </div>
              <div className="uml-item">
                <h5>Sequence Diagram</h5>
                <img src="/images/SequenceDiagram.png" alt="Sequence Diagram" />
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProjectModal;
