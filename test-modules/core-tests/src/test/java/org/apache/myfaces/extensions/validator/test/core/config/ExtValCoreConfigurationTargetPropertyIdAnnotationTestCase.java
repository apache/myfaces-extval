package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.CustomConstraintSource;
import org.apache.myfaces.extensions.validator.test.core.config.support.CustomTargetPropertyId;
import org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils;
import org.apache.myfaces.extensions.validator.util.ExtValAnnotationUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

public class ExtValCoreConfigurationTargetPropertyIdAnnotationTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationTargetPropertyIdAnnotationTestCase(String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
        }
    }

    @Override
    protected void invokeStartupListeners()
    {
        super.invokeStartupListeners();

        // This is the normal place where the user can intervene in the startup
        // and change the config
        if (needCustomConfig())
        {

            ExtValCoreConfiguration.use(new DefaultExtValCoreConfiguration()
            {

                @Override
                public Class<? extends Annotation> targetPropertyIdAnnotation()
                {
                    return CustomTargetPropertyId.class;
                }

            }, true);
        }
    }

    public void testTargetPropertyIdAnnotationDefault() throws Exception
    {
        Method method = ReflectionUtils.getMethod(ConstraintSourceUtils.class,
                "getTargetPropertyMetaData", Class.class, String.class);
        assertNotNull(method);
        Annotation target = (Annotation) ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property5");

        assertEquals(ConstraintSource.class.getName(),
                ((Class) ExtValAnnotationUtils.extractValueOf(target,
                        Object.class)).getName());

        assertNull(ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property6"));
    }

    public void testTargetPropertyIdAnnotationCustomConfig() throws Exception
    {
        Method method = ReflectionUtils.getMethod(ConstraintSourceUtils.class,
                "getTargetPropertyMetaData", Class.class, String.class);
        assertNotNull(method);
        Annotation target = (Annotation) ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property6");

        assertEquals(CustomConstraintSource.class.getName(),
                ((Class) ExtValAnnotationUtils.extractValueOf(target,
                        Object.class)).getName());

        assertNull(ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property5"));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationTargetPropertyIdAnnotationTestCase.class);
    }

}
