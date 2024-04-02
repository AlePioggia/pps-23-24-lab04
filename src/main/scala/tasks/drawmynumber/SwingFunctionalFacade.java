package tasks.drawmynumber;

import java.awt.FlowLayout;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Supplier;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

class SwingFunctionalFacade {
    public static interface Frame {
        Frame setSize(int width, int height);

        Frame addButton(String text, String name);

        Frame addLabel(String text, String name);

        Frame showToLabel(String text, String name);

        Frame addTextField();

        Frame show();

        Supplier<String> events();
    }

    public static Frame createFrame() {
        return new FrameImpl();
    }

    private static class FrameImpl implements Frame {
        private final JFrame jframe = new JFrame();
        private JButton button = new JButton();
        private JLabel label = new JLabel();
        private JTextField textField = new JTextField();
        private final LinkedBlockingQueue<String> eventQueue = new LinkedBlockingQueue<>();
        private final Supplier<String> events = () -> {
            try {
                return eventQueue.take();
            } catch (InterruptedException e) {
                return "";
            }
        };

        public FrameImpl() {
            this.jframe.setLayout(new FlowLayout());
        }

        @Override
        public Frame setSize(int width, int height) {
            this.jframe.setSize(width, height);
            return this;
        }

        @Override
        public Frame addButton(String text, String name) {
            this.button = new JButton(text);
            this.button.setActionCommand(name);
            this.button.addActionListener(e -> {
                try {
                    eventQueue.put(this.textField.getText());
                } catch (InterruptedException ex) {
                }
            });
            this.jframe.getContentPane().add(this.button);
            return this;
        }

        @Override
        public Frame addLabel(String text, String name) {
            this.label = new JLabel(text);
            this.label.setName(name);
            this.jframe.getContentPane().add(this.label);
            return this;
        }

        @Override
        public Frame showToLabel(String text, String name) {
            this.label.setText(text);
            return this;
        }

        @Override
        public Frame addTextField() {
            this.textField = new JTextField();
            this.textField.setColumns(10);
            this.jframe.getContentPane().add(this.textField);
            return this;
        }

        @Override
        public Frame show() {
            this.jframe.setVisible(true);
            return this;
        }

        @Override
        public Supplier<String> events() {
            return this.events;
        }

    }
}
