package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.el.DefaultELHelper;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareBean;

import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.component.html.HtmlInputText;

public class ExtValCoreConfigurationDeactivateElResolverTestCase extends
        ExtValCoreConfigurationTestCase
{
    private UIInput uiComponent;

    public ExtValCoreConfigurationDeactivateElResolverTestCase(String name)
    {
        super(name);
    }

    public static class CustomDefaultELHelper extends DefaultELHelper
    {

        @Override
        protected PropertyDetails getPropertyDetailsViaReflectionFallback(
                UIComponent uiComponent)
        {
            return null;
        }

    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        uiComponent = new HtmlInputText();
        ConstraintSourceAwareBean bean = new ConstraintSourceAwareBean();

        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

        createValueBinding(uiComponent, "value", "#{testBean.property1}");
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".DEACTIVATE_EL_RESOLVER", "true");
        }
    }

    public void testDeactivateElResolverDefault()
    {
        ELHelper elHelper = new CustomDefaultELHelper();
        assertNotNull(elHelper.getPropertyDetailsOfValueBinding(uiComponent));

    }

    public void testDeactivateElResolverWebXml()
    {
        ELHelper elHelper = new CustomDefaultELHelper();
        // When deactivated, the getPropertyDetailsViaReflectionFallback method
        // is called which returns null in our custom version
        assertNull(elHelper.getPropertyDetailsOfValueBinding(uiComponent));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationDeactivateElResolverTestCase.class);
    }

}
