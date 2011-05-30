package org.apache.myfaces.extensions.validator.test.propval.constraintsource;

import javax.faces.component.UIInput;
import javax.faces.component.UIViewRoot;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputText;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.test.propval.AbstractPropertyValidationTestCase;
import org.junit.Assert;

@ToDo(value=Priority.MEDIUM,description="This is more or less the same as BaseBeanValPropertyValidationTestCase but can it be unified ??")
public abstract class AbstractConstraintSourceTestCase<T> extends AbstractPropertyValidationTestCase
{
    
    protected HtmlInputText inputComponent1 = null;
    protected HtmlInputText inputComponent2 = null;
    protected HtmlInputText inputComponent3 = null;

    private UIViewRoot rootComponent = null;

    protected T bean;


    @SuppressWarnings({"UnusedDeclaration"})
    @Override
    protected void setUpTestCase()
    {
        super.setUpTestCase();
        bean = getBeanToTest();
        bindBeanToExpression();

        createComponents();

    }

    private void createComponents()
    {
        rootComponent = new UIViewRoot();
        HtmlForm form = new HtmlForm();
        form.setId("form");
        rootComponent.getChildren().add(form);
        inputComponent1 = new HtmlInputText();
        inputComponent2 = new HtmlInputText();
        inputComponent3 = new HtmlInputText();
        form.getChildren().add(inputComponent1);
        form.getChildren().add(inputComponent2);
        form.getChildren().add(inputComponent3);
        inputComponent1.setId("input1");
        inputComponent2.setId("input2");
        inputComponent3.setId("input3");
    }

    @SuppressWarnings({"UnusedDeclaration"})
    private void bindBeanToExpression()
    {
        ValueBinding vb = application.createValueBinding("#{testBean}");
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);
    }

    protected abstract T getBeanToTest();

    @Override
    protected void resetTestCase()
    {
        super.resetTestCase();
        inputComponent1 = null;
        inputComponent2 = null;
        inputComponent3 = null;
        rootComponent = null;
        bean = null;
    }

    protected void createValueBindingForComponent(UIInput uiComponent, String valueBinding)
    {
        createValueBinding(uiComponent, "value", valueBinding);
    }

    protected void setValueToValidate(UIInput uiComponent, String valueToValidate)
    {
        uiComponent.setSubmittedValue(valueToValidate);
    }

    protected void validateComponents()
    {
        inputComponent1.processValidators(facesContext);
        inputComponent2.processValidators(facesContext);
        inputComponent3.processValidators(facesContext);
    }

    protected void updateComponents()
    {
        inputComponent1.processUpdates(facesContext);
        inputComponent2.processUpdates(facesContext);
        inputComponent3.processUpdates(facesContext);
    }

    protected void assertComponentValid(UIInput uiInput)
    {
        Assert.assertTrue(isComponentValid(uiInput));
    }

    protected void assertComponentInvalid(UIInput uiInput)
    {
        Assert.assertFalse(isComponentValid(uiInput));
    }

    private boolean isComponentValid(UIInput uiComponent)
    {
        return uiComponent.isValid();
    }
    

}
