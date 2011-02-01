package org.apache.myfaces.extensions.validator.test.propval.crossval;

import org.apache.myfaces.extensions.validator.test.propval.AbstractPropertyValidationTestCase;
import org.junit.Test;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIViewRoot;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputText;
import javax.faces.convert.DateTimeConverter;

/**
 * @author Rudy De Busscher
 */
public class DateIs3TestCase extends AbstractPropertyValidationTestCase
{

    HtmlInputText inputComponent1 = null;
    HtmlInputText inputComponent2 = null;

    UIViewRoot rootComponent = null;

    public DateIs3TestCase()
    {
        inputComponent1 = null;
        inputComponent2 = null;
        rootComponent = null;
    }


    @Override
    protected void setUpTestCase()
    {
        super.setUpTestCase();
        DateIs3TestBean bean = new DateIs3TestBean();
        createValueBinding(null, "value", "#{testBean}");
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

        rootComponent = new UIViewRoot();
        HtmlForm form = new HtmlForm();
        form.setId("form");
        rootComponent.getChildren().add(form);
        inputComponent1 = new HtmlInputText();
        form.getChildren().add(inputComponent1);
        inputComponent1.setId("input1");
        inputComponent2 = new HtmlInputText();
        form.getChildren().add(inputComponent2);
        inputComponent2.setId("input2");

        DateTimeConverter converter = new DateTimeConverter();
        converter.setPattern("DD/MM/yyyy");
        inputComponent1.setConverter(converter);
        inputComponent2.setConverter(converter);
    }

    @Test
    public void testDateIsSameCorrect() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue("25/01/2011");
        inputComponent2.setSubmittedValue("25/01/2011");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(0);

        assertNavigationBlocked(false);
    }

    @Test
    public void testDateIsSameWrong() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue("23/01/2011");
        inputComponent2.setSubmittedValue("25/01/2011");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(1);

        assertNavigationBlocked(true);

        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    @Test
    public void testDateIsSameTargetNull() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue("");
        inputComponent2.setSubmittedValue("25/01/2011");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(1);

        assertNavigationBlocked(true);

        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);


    }

    @Test
    public void testDateIssSameSourceNull() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue("25/01/2011");
        inputComponent2.setSubmittedValue("");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(0);

        assertNavigationBlocked(false);

    }

}