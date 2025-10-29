function Component({ name, ...props }) {
  return <div {...props}>Hello {name}!</div>;
}

export default Component;
