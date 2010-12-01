package org.apache.myfaces.extensions.validator.test.propval.constraintsource;

import javax.faces.application.FacesMessage;

import org.apache.myfaces.extensions.validator.test.propval.constraintsource.model.ConstraintSourceAware2Bean;
import org.junit.Test;

public class ConstraintSourceAwareValidation2TestCase extends
AbstractConstraintSourceTestCase<ConstraintSourceAware2Bean>
{

    protected ConstraintSourceAware2Bean getBeanToTest()
    {
        return new ConstraintSourceAware2Bean();
    }

    @Test
    public void testIgnoreConstraintSource()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

    @Test
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
