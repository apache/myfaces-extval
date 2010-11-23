package org.apache.myfaces.extensions.validator.test.propval.constraintsource;

import javax.faces.application.FacesMessage;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.extensions.validator.test.propval.constraintsource.model.ConstraintSourceAware2Bean;

public class ConstraintSourceAwareValidation2TestCase extends
AbstractConstraintSourceTestCase<ConstraintSourceAware2Bean>
{
    public ConstraintSourceAwareValidation2TestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(ConstraintSourceAwareValidation2TestCase.class);
    }

    protected ConstraintSourceAware2Bean getBeanToTest()
    {
        return new ConstraintSourceAware2Bean();
    }

    public void testIgnoreConstraintSource()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

    public void testStringBasedTargetProperty()
    {
        createValueBindingForComponent(this.inputComponent2, "#{testBean.property2}");
        setValueToValidate(this.inputComponent2, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

}
