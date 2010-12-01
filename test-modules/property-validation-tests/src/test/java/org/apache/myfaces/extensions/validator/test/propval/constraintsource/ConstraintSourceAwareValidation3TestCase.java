package org.apache.myfaces.extensions.validator.test.propval.constraintsource;

import javax.faces.application.FacesMessage;

import org.apache.myfaces.extensions.validator.test.propval.constraintsource.model.ConstraintSourceAware3Bean;
import org.junit.Test;

public class ConstraintSourceAwareValidation3TestCase extends
AbstractConstraintSourceTestCase<ConstraintSourceAware3Bean>
{

    protected ConstraintSourceAware3Bean getBeanToTest()
    {
        return new ConstraintSourceAware3Bean();
    }

    @Test
    public void testMissingBasedConstraintSource()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

    @Test
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
