package org.apache.myfaces.extensions.validator.test.propval.constraintsource;

import javax.faces.application.FacesMessage;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.IgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.TargetProperty;
import org.apache.myfaces.extensions.validator.core.validation.TargetPropertyId;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomConstraintSource;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomIgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomTargetProperty;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomTargetPropertyId;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.model.ConstraintSourceAware6Bean;

public class ConstraintSourceAwareValidation6TestCase extends
        AbstractConstraintSourceTestCase<ConstraintSourceAware6Bean>
{
    public ConstraintSourceAwareValidation6TestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(ConstraintSourceAwareValidation6TestCase.class);
    }

    protected ConstraintSourceAware6Bean getBeanToTest()
    {
        return new ConstraintSourceAware6Bean();
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        ExtValContext extValContext = ExtValContext.getContext();

        extValContext.addGlobalProperty(ConstraintSource.class.getName(),
                CustomConstraintSource.class);
        extValContext.addGlobalProperty(IgnoreConstraintSource.class.getName(),
                CustomIgnoreConstraintSource.class);
        extValContext.addGlobalProperty(TargetProperty.class.getName(),
                CustomTargetProperty.class);
        extValContext.addGlobalProperty(TargetPropertyId.class.getName(),
                CustomTargetPropertyId.class);
    }

    public void testCustomAnnotations1()
    {
        createValueBindingForComponent(this.inputComponent1,
                "#{testBean.property1}");
        setValueToValidate(this.inputComponent1, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    public void testCustomAnnotations2()
    {
        createValueBindingForComponent(this.inputComponent2,
                "#{testBean.property2}");
        setValueToValidate(this.inputComponent2, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    public void testCustomAnnotations3()
    {
        createValueBindingForComponent(this.inputComponent3,
                "#{testBean.property3}");
        setValueToValidate(this.inputComponent3, "");

        validateComponents();

        assertComponentValid(this.inputComponent3);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

}
