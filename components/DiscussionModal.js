"use client";

import { aclDiscussionData } from '../data/aclDiscussion';

const DiscussionModal = ({ onClose }) => {
  const mainPost = aclDiscussionData[0];
  const replies = aclDiscussionData.slice(1);

  return (
    <div className="modal-container active">
      <div className="overlay active" onClick={onClose}></div>
      <div className="testimonials-modal">
        <button className="modal-close-btn" onClick={onClose}>
          <ion-icon name="close-outline"></ion-icon>
        </button>

        <div className="modal-content">
          <h4 className="h3 modal-title">Agent Communication Languages - Discussion</h4>
          <div className="discussion-thread">
            {/* Main Post */}
            <div className="discussion-post main-post">
              <div className="post-author">{mainPost.author}</div>
              <div className="post-content">
                {mainPost.content.map((paragraph, i) => (
                  <p key={i}>{paragraph}</p>
                ))}
                {mainPost.references && (
                  <div className="references">
                    <h5>References</h5>
                    <ul>
                      {mainPost.references.map((ref, i) => (
                        <li key={i}>{ref}</li>
                      ))}
                    </ul>
                  </div>
                )}
              </div>
            </div>

            {/* Replies */}
            <div className="discussion-replies">
              {replies.map((post, index) => (
                <div key={index} className="discussion-post reply-post">
                  <div className="post-author">{post.author}</div>
                  <div className="post-content">
                    {post.content.map((paragraph, i) => (
                      <p key={i}>{paragraph}</p>
                    ))}
                    {post.references && (
                      <div className="references">
                        <h5>References</h5>
                        <ul>
                          {post.references.map((ref, i) => (
                            <li key={i}>{ref}</li>
                          ))}
                        </ul>
                      </div>
                    )}
                  </div>
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default DiscussionModal;
