package org.apache.myfaces.extensions.validator.test.propval.constraintsource;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomConstraintSource;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomIgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomTargetProperty;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomTargetPropertyId;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.model.ConstraintSourceAware6Bean;
import org.junit.Test;

import javax.faces.application.FacesMessage;
import java.lang.annotation.Annotation;

public class ConstraintSourceAwareValidation6TestCase extends
        AbstractConstraintSourceTestCase<ConstraintSourceAware6Bean>
{

    protected ConstraintSourceAware6Bean getBeanToTest()
    {
        return new ConstraintSourceAware6Bean();
    }

    
    @Override
    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        return new DefaultExtValCoreConfiguration() {
            @Override
            public Class<? extends Annotation> constraintSourceAnnotation()
            {
                return CustomConstraintSource.class;
            }

            @Override
            public Class<? extends Annotation> ignoreConstraintSourceAnnotation()
            {
                return CustomIgnoreConstraintSource.class;
            }

            @Override
            public Class<? extends Annotation> targetPropertyAnnotation()
            {
                return CustomTargetProperty.class;
            }

            @Override
            public Class<? extends Annotation> targetPropertyIdAnnotation()
            {
                return CustomTargetPropertyId.class;
            }
        };
    }

    @Test
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

    @Test
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

    @Test
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
