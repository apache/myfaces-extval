package org.apache.myfaces.extensions.validator.test.propval.constraintsource;

import javax.faces.application.FacesMessage;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.extensions.validator.test.propval.constraintsource.model.ConstraintSourceAware4Bean;

public class ConstraintSourceAwareValidation4TestCase extends
AbstractConstraintSourceTestCase<ConstraintSourceAware4Bean>
{
    public ConstraintSourceAwareValidation4TestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(ConstraintSourceAwareValidation4TestCase.class);
    }

    protected ConstraintSourceAware4Bean getBeanToTest()
    {
        return new ConstraintSourceAware4Bean();
    }

    public void testMethodBasedConstraintSourceAndTargetPropertyAnnotation()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    public void testMissingBasedConstraintSource()
    {
        createValueBindingForComponent(this.inputComponent2, "#{testBean.property2}");
        setValueToValidate(this.inputComponent2, "");

        validateComponents();

        assertComponentValid(this.inputComponent2);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

}
