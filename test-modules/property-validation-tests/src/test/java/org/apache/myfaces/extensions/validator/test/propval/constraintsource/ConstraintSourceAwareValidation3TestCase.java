package org.apache.myfaces.extensions.validator.test.propval.constraintsource;

import javax.faces.application.FacesMessage;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.extensions.validator.test.propval.constraintsource.model.ConstraintSourceAware3Bean;

public class ConstraintSourceAwareValidation3TestCase extends
AbstractConstraintSourceTestCase<ConstraintSourceAware3Bean>
{
    public ConstraintSourceAwareValidation3TestCase(String name)
    {
        super(name);
    }
    public static Test suite()
    {
        return new TestSuite(ConstraintSourceAwareValidation3TestCase.class);
    }

    protected ConstraintSourceAware3Bean getBeanToTest()
    {
        return new ConstraintSourceAware3Bean();
    }

    public void testMissingBasedConstraintSource()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

    public void testFieldBasedConstraintSource()
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
